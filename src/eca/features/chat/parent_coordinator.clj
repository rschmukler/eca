(ns eca.features.chat.parent-coordinator
  "Serializes questions from sibling subagents against a snapshot of their parent context."
  (:require
   [clojure.string :as string]
   [eca.features.chat.lifecycle :as lifecycle]
   [eca.llm-api :as llm-api]
   [eca.logger :as logger]
   [eca.shared :as shared])
  (:import
   [java.util.concurrent.locks ReentrantLock]))

(set! *warn-on-reflection* true)

(def ^:private logger-tag "[PARENT-COORDINATOR]")
(def ^:private max-summary-length 4000)

(def ^:private coordinator-instructions
  (str "<ask-parent-coordinator>\n"
       "You are a private proxy for the parent coding agent while its delegated subagents run. "
       "Answer each subagent question using the parent conversation and instructions supplied to you. "
       "Questions arrive serially, and earlier questions and answers are part of your context. "
       "Preserve decisions and assumptions across answers. Be concise and decisive. "
       "If the parent context does not establish an answer, say so and recommend the safest explicit assumption. "
       "Do not claim to have asked the user, run tools, or inspected anything outside the supplied context.\n"
       "</ask-parent-coordinator>"))

(def ^:private summary-prompt
  (str "Summarize only the decisions, clarifications, and assumptions established through the subagent "
       "questions and your answers. This summary will be added to the parent agent's context alongside the "
       "subagents' own results. Avoid repeating child work, omit conversational filler, and stay under 1200 characters."))

(defn ^:private append-instructions
  [instructions]
  (if (map? instructions)
    (update instructions :dynamic
            (fn [dynamic]
              (str (when (seq dynamic) (str dynamic "\n\n"))
                   coordinator-instructions)))
    (str (when (seq instructions) (str instructions "\n\n"))
         coordinator-instructions)))

(defn ^:private bounded-text
  [value limit]
  (let [text (str value)]
    (if (> (count text) limit)
      (str (subs text 0 (- limit 3)) "...")
      text)))

(defn ^:private runtime
  [db coordinator-id]
  (get-in db [:parent-coordinators coordinator-id]))

(defn active?
  "Returns whether a coordinator currently accepts subagent questions."
  [db coordinator-id]
  (when-let [coordinator (runtime db coordinator-id)]
    (and @(:accepting?* coordinator)
         (not @(:cancelled?* coordinator)))))

(defn ask-parent-enabled?
  "Returns whether ask_parent should be exposed for a child chat."
  [db chat-id]
  (let [chat (get-in db [:chats chat-id])
        coordinator-id (:parent-coordinator-id chat)
        max-questions (get-in chat [:subagent :max-questions] 0)]
    (and (:subagent chat)
         (integer? max-questions)
         (pos? max-questions)
         coordinator-id
         (active? db coordinator-id))))

(defn create!
  "Creates a lightweight coordinator for one parent tool-call batch."
  [{:keys [db* chat-id prompt-id provider model model-capabilities variant instructions config]
    :as chat-ctx}]
  (let [coordinator-id (str (random-uuid))
        coordinator {:id coordinator-id
                     :db* db*
                     :chat-ctx chat-ctx
                     :parent-chat-id chat-id
                     :parent-prompt-id prompt-id
                     :provider provider
                     :model model
                     :model-capabilities model-capabilities
                     :variant variant
                     :instructions (append-instructions instructions)
                     :config config
                     :lock (ReentrantLock. true)
                     :messages* (atom (vec (shared/messages-after-last-compact-marker
                                            (get-in @db* [:chats chat-id :messages] []))))
                     :children* (atom {})
                     :question-counts* (atom {})
                     :qa-log* (atom [])
                     :accepting?* (atom true)
                     :cancelled?* (atom false)}]
    (swap! db* assoc-in [:parent-coordinators coordinator-id] coordinator)
    coordinator-id))

(defn register-child!
  "Associates an eligible spawned child with a coordinator."
  [db* coordinator-id child-chat-id child]
  (when-let [coordinator (runtime @db* coordinator-id)]
    (when (active? @db* coordinator-id)
      (swap! (:children* coordinator) assoc child-chat-id child)
      true)))

(defn ^:private parent-cancelled?
  [coordinator]
  (let [chat (get-in @(:db* coordinator) [:chats (:parent-chat-id coordinator)])]
    (or @(:cancelled?* coordinator)
        (.isInterrupted (Thread/currentThread))
        (= :stopping (:status chat))
        (:prompt-finished? chat)
        (not= (:parent-prompt-id coordinator) (:prompt-id chat)))))

(defn ^:private reserve-question!
  [coordinator child-chat-id max-questions]
  (let [counts* (:question-counts* coordinator)]
    (loop []
      (let [counts @counts*
            current (long (get counts child-chat-id 0))]
        (cond
          (>= current max-questions) nil
          (compare-and-set! counts* counts (assoc counts child-chat-id (inc current))) (inc current)
          :else (recur))))))

(defn ^:private error-message
  [error]
  (or (some-> error :exception ex-message)
      (:message error)
      "The parent-context model call failed."))

(defn ^:private call-model!
  [coordinator user-message]
  (when (parent-cancelled? coordinator)
    (throw (InterruptedException. "Parent prompt stopped")))
  (lifecycle/maybe-renew-auth-token (:chat-ctx coordinator))
  (let [result (llm-api/sync-prompt!
                {:provider (:provider coordinator)
                 :model (:model coordinator)
                 :model-capabilities (:model-capabilities coordinator)
                 :instructions (:instructions coordinator)
                 :past-messages @(:messages* coordinator)
                 :user-messages [user-message]
                 :config (:config coordinator)
                 :tools nil
                 :provider-auth (get-in @(:db* coordinator) [:auth (:provider coordinator)])
                 :variant (:variant coordinator)
                 :cancelled? #(parent-cancelled? coordinator)
                 :subagent? true})]
    (when (parent-cancelled? coordinator)
      (throw (InterruptedException. "Parent prompt stopped")))
    (when-let [error (:error result)]
      (throw (ex-info (error-message error) {:error error})))
    (or (some-> (:output-text result) string/trim not-empty)
        (throw (ex-info "The parent-context model returned an empty response." {})))))

(defn ask!
  "Answers one child question from the serialized parent-context conversation."
  [db* child-chat-id question]
  (let [db @db*
        child-chat (get-in db [:chats child-chat-id])
        coordinator-id (:parent-coordinator-id child-chat)
        coordinator (runtime db coordinator-id)
        max-questions (get-in child-chat [:subagent :max-questions] 0)]
    (cond
      (not (and coordinator-id coordinator))
      {:error true
       :message "The parent-context coordinator is no longer available."}

      (not (active? db coordinator-id))
      {:error true
       :message "The parent-context coordinator is no longer accepting questions."}

      (not (and (integer? max-questions) (pos? max-questions)))
      {:error true
       :message "This subagent is not configured to ask the parent."}

      :else
      (if-let [question-number (reserve-question! coordinator child-chat-id max-questions)]
        (let [child (or (get @(:children* coordinator) child-chat-id)
                        {:agent-name (:agent-name child-chat)
                         :task (:subagent-task child-chat)})
              ^ReentrantLock lock (:lock coordinator)]
          (.lockInterruptibly lock)
          (try
            (if-not (active? @db* coordinator-id)
              {:error true
               :message "The parent-context coordinator stopped before answering."}
              (logger/with-chat-context (:parent-chat-id coordinator) nil
                (let [question-message {:role "user"
                                        :content [{:type :text
                                                   :text (format (str "<subagent-question from=\"%s\" number=\"%d\">\n"
                                                                      "Task: %s\n\nQuestion: %s\n"
                                                                      "</subagent-question>")
                                                                 (or (:agent-name child) child-chat-id)
                                                                 question-number
                                                                 (or (:task child) "Not provided")
                                                                 question)}]}
                      answer (call-model! coordinator question-message)
                      answer-message {:role "assistant"
                                      :content [{:type :text :text answer}]}]
                  (swap! (:messages* coordinator) into [question-message answer-message])
                  (swap! (:qa-log* coordinator) conj {:child-chat-id child-chat-id
                                                      :agent-name (:agent-name child)
                                                      :question question
                                                      :answer answer})
                  {:error false :answer answer})))
            (finally
              (.unlock lock))))
        {:error true
         :message (format "This subagent has already used its maximum of %d parent questions."
                          max-questions)}))))

(defn ^:private fallback-summary
  [qa-log]
  (->> qa-log
       (map (fn [{:keys [agent-name question answer]}]
              (format "- %s asked: %s\n  Answer: %s"
                      (or agent-name "Subagent")
                      (bounded-text question 300)
                      (bounded-text answer 700))))
       (string/join "\n")
       (#(bounded-text % max-summary-length))))

(defn complete!
  "Stops accepting questions and returns a compact summary when any were answered."
  [db* coordinator-id]
  (when-let [coordinator (runtime @db* coordinator-id)]
    (reset! (:accepting?* coordinator) false)
    (let [qa-log @(:qa-log* coordinator)]
      (when (seq qa-log)
        (let [^ReentrantLock lock (:lock coordinator)]
          (.lockInterruptibly lock)
          (try
            (logger/with-chat-context (:parent-chat-id coordinator) nil
              (try
                (-> (call-model! coordinator
                                 {:role "user"
                                  :content [{:type :text :text summary-prompt}]})
                    (bounded-text max-summary-length))
                (catch InterruptedException e
                  (throw e))
                (catch Exception e
                  (logger/warn logger-tag "Falling back to a deterministic coordination summary" e)
                  (fallback-summary qa-log))))
            (finally
              (.unlock lock))))))))

(defn dispose!
  "Cancels and removes coordinator runtime state."
  [db* coordinator-id]
  (when-let [coordinator (runtime @db* coordinator-id)]
    (reset! (:accepting?* coordinator) false)
    (reset! (:cancelled?* coordinator) true)
    (swap! db* update :parent-coordinators dissoc coordinator-id)))
