(ns eca.features.chat.parent-coordinator-test
  (:require
   [clojure.string :as string]
   [clojure.test :refer [deftest is testing]]
   [eca.features.chat.lifecycle :as lifecycle]
   [eca.features.chat.parent-coordinator :as parent-coordinator]
   [eca.llm-api :as llm-api]
   [eca.test-helper :as h]
   [matcher-combinators.test :refer [match?]]))

(h/reset-components-before-test)

(defn ^:private coordinator-context
  [db*]
  {:db* db*
   :chat-id "parent"
   :prompt-id "prompt-1"
   :provider "test"
   :model "model"
   :model-capabilities {:tools true}
   :variant "high"
   :instructions {:static "Parent instructions" :dynamic "Parent context"}
   :config {}
   :messenger (h/messenger)
   :metrics (h/metrics)})

(defn ^:private setup-coordinator!
  [db* child-configs]
  (swap! db* assoc-in [:chats "parent"]
         {:id "parent"
          :prompt-id "prompt-1"
          :status :running
          :messages [{:role "user"
                      :content [{:type :text :text "Preserve compatibility."}]}]})
  (let [coordinator-id (parent-coordinator/create! (coordinator-context db*))]
    (doseq [[child-chat-id {:keys [agent-name task max-questions]}] child-configs]
      (is (true? (parent-coordinator/register-child!
                  db* coordinator-id child-chat-id {:agent-name agent-name :task task})))
      (swap! db* assoc-in [:chats child-chat-id]
             {:id child-chat-id
              :parent-chat-id "parent"
              :parent-coordinator-id coordinator-id
              :agent-name agent-name
              :subagent-task task
              :subagent {:max-questions max-questions}}))
    coordinator-id))

(deftest serial-questions-share-context-test
  (testing "parallel child questions are processed one at a time and later answers see earlier Q/A"
    (let [db* (h/db*)
          coordinator-id (setup-coordinator!
                          db*
                          {"child-a" {:agent-name "a" :task "Implement A" :max-questions 1}
                           "child-b" {:agent-name "b" :task "Implement B" :max-questions 1}})
          first-entered* (promise)
          release-first* (promise)
          calls* (atom [])]
      (with-redefs [lifecycle/maybe-renew-auth-token (fn [_] nil)
                    llm-api/sync-prompt!
                    (fn [{:keys [past-messages user-messages variant tools instructions]}]
                      (let [question (get-in user-messages [0 :content 0 :text])
                            call-number (inc (count @calls*))]
                        (swap! calls* conj {:past-messages past-messages
                                            :question question
                                            :variant variant
                                            :tools tools
                                            :instructions instructions})
                        (when (= 1 call-number)
                          (deliver first-entered* true)
                          @release-first*)
                        {:output-text (str "answer-" call-number)}))]
        (let [answer-a* (future (parent-coordinator/ask! db* "child-a" "Should A preserve compatibility?"))]
          @first-entered*
          (let [answer-b* (future (parent-coordinator/ask! db* "child-b" "Should B follow the same decision?"))]
            (Thread/sleep 50)
            (is (= 1 (count @calls*)) "the second model call waits for the first answer")
            (deliver release-first* true)
            (is (match? {:error false :answer "answer-1"} @answer-a*))
            (is (match? {:error false :answer "answer-2"} @answer-b*))))

        (is (= "high" (:variant (first @calls*))))
        (is (nil? (:tools (first @calls*))))
        (is (string/includes? (get-in (first @calls*) [:instructions :dynamic])
                              "private proxy for the parent coding agent"))
        (is (some #(and (= "assistant" (:role %))
                        (= "answer-1" (get-in % [:content 0 :text])))
                  (:past-messages (second @calls*)))
            "the second answer receives the first answer in coordinator history"))
      (parent-coordinator/dispose! db* coordinator-id))))

(deftest waiting-question-cancellation-test
  (testing "a child waiting for the serialization lock can be interrupted"
    (let [db* (h/db*)
          coordinator-id (setup-coordinator!
                          db*
                          {"child-a" {:agent-name "a" :task "Implement A" :max-questions 1}
                           "child-b" {:agent-name "b" :task "Implement B" :max-questions 1}})
          first-entered* (promise)
          release-first* (promise)
          calls* (atom 0)]
      (with-redefs [lifecycle/maybe-renew-auth-token (fn [_] nil)
                    llm-api/sync-prompt! (fn [_]
                                           (let [call-number (swap! calls* inc)]
                                             (when (= 1 call-number)
                                               (deliver first-entered* true)
                                               @release-first*)
                                             {:output-text "answer"}))]
        (let [first-answer* (future (parent-coordinator/ask! db* "child-a" "First?"))]
          @first-entered*
          (let [waiting-answer* (future (parent-coordinator/ask! db* "child-b" "Second?"))]
            (Thread/sleep 50)
            (is (true? (future-cancel waiting-answer*)))
            (deliver release-first* true)
            (is (false? (:error @first-answer*)))
            (Thread/sleep 20)
            (is (= 1 @calls*) "the cancelled waiter never starts a model call"))))
      (parent-coordinator/dispose! db* coordinator-id))))

(deftest max-questions-test
  (testing "the per-child question limit is consumed when a question is accepted"
    (let [db* (h/db*)
          coordinator-id (setup-coordinator!
                          db*
                          {"child-a" {:agent-name "a" :task "Implement A" :max-questions 1}})
          calls* (atom 0)]
      (with-redefs [lifecycle/maybe-renew-auth-token (fn [_] nil)
                    llm-api/sync-prompt! (fn [_]
                                           (swap! calls* inc)
                                           {:output-text "Use compatibility mode."})]
        (is (match? {:error false :answer "Use compatibility mode."}
                    (parent-coordinator/ask! db* "child-a" "Which mode?")))
        (is (match? {:error true
                     :message #"maximum of 1"}
                    (parent-coordinator/ask! db* "child-a" "One more question?")))
        (is (= 1 @calls*)))
      (parent-coordinator/dispose! db* coordinator-id))))

(deftest complete-coordinator-test
  (testing "no questions means no summary model call"
    (let [db* (h/db*)
          coordinator-id (setup-coordinator! db* {})]
      (with-redefs [llm-api/sync-prompt! (fn [_]
                                           (throw (ex-info "should not be called" {})))]
        (is (nil? (parent-coordinator/complete! db* coordinator-id))))
      (parent-coordinator/dispose! db* coordinator-id)
      (is (nil? (get-in @db* [:parent-coordinators coordinator-id])))))

  (testing "answered questions produce a final compact summary"
    (let [db* (h/db*)
          coordinator-id (setup-coordinator!
                          db*
                          {"child-a" {:agent-name "a" :task "Implement A" :max-questions 1}})
          calls* (atom 0)]
      (with-redefs [lifecycle/maybe-renew-auth-token (fn [_] nil)
                    llm-api/sync-prompt! (fn [{:keys [user-messages]}]
                                           (let [call-number (swap! calls* inc)
                                                 text (get-in user-messages [0 :content 0 :text])]
                                             (if (= 1 call-number)
                                               {:output-text "Preserve the old behavior."}
                                               (do
                                                 (is (string/includes? text "Summarize only the decisions"))
                                                 {:output-text "Preserve the old behavior for compatibility."}))))]
        (is (false? (:error (parent-coordinator/ask! db* "child-a" "Old or new behavior?"))))
        (is (= "Preserve the old behavior for compatibility."
               (parent-coordinator/complete! db* coordinator-id)))
        (is (= 2 @calls*)))
      (parent-coordinator/dispose! db* coordinator-id))))
