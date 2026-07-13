(ns eca.features.tools.ask-parent
  (:require
   [clojure.string :as string]
   [eca.db :as db]
   [eca.features.chat.parent-coordinator :as parent-coordinator]
   [eca.features.tools.util :as tools.util]
   [eca.logger :as logger]))

(set! *warn-on-reflection* true)

(def ^:private logger-tag "[TOOLS-PARENT]")

(defn ^:private ask-parent
  [arguments {:keys [db* chat-id]}]
  (let [question (some-> (get arguments "question") string/trim)]
    (if (string/blank? question)
      (tools.util/single-text-content "INVALID_ARGS: `question` is required and must not be blank." :error)
      (try
        (let [{:keys [error answer message]} (parent-coordinator/ask! db* chat-id question)]
          (if error
            (tools.util/single-text-content message :error)
            (tools.util/single-text-content answer)))
        (catch InterruptedException _
          (.interrupt (Thread/currentThread))
          (tools.util/single-text-content "The parent question was cancelled because the parent prompt stopped." :error))
        (catch Exception e
          (logger/with-chat-context chat-id (db/parent-chat-id @db* chat-id)
            (logger/error logger-tag "Error asking parent-context coordinator" e))
          (tools.util/single-text-content
           (str "The parent-context coordinator could not answer: " (or (ex-message e) "unknown error"))
           :error))))))

(def definitions
  {"ask_parent"
   {:description (tools.util/read-tool-description "ask_parent")
    :parameters {:type "object"
                 :properties {"question" {:type "string"
                                           :description "A concise decision or clarification question that depends on the parent conversation."}}
                 :required ["question"]}
    :handler #'ask-parent
    :enabled-fn (fn [{:keys [db chat-id]}]
                  (parent-coordinator/ask-parent-enabled? db chat-id))
    :auto-approve? true
    :summary-fn (fn [{:keys [args]}]
                  (if-let [question (some-> (get args "question") string/trim not-empty)]
                    (str "Asking parent: " (if (> (count question) 45)
                                             (str (subs question 0 42) "...")
                                             question))
                    "Asking parent"))}})
