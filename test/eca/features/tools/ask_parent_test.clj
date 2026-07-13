(ns eca.features.tools.ask-parent-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [eca.features.chat.parent-coordinator :as parent-coordinator]
   [eca.features.tools.ask-parent :as f.tools.ask-parent]
   [eca.test-helper :as h]
   [matcher-combinators.test :refer [match?]]))

(h/reset-components-before-test)

(defn ^:private call-ask-parent
  [arguments]
  ((get-in f.tools.ask-parent/definitions ["ask_parent" :handler])
   arguments
   {:db* (h/db*)
    :chat-id "child"}))

(deftest ask-parent-test
  (testing "returns the coordinator answer"
    (with-redefs [parent-coordinator/ask! (fn [_db* chat-id question]
                                            (is (= "child" chat-id))
                                            (is (= "Which API should I preserve?" question))
                                            {:error false :answer "Preserve the existing public API."})]
      (is (match? {:error false
                   :contents [{:type :text :text "Preserve the existing public API."}]}
                  (call-ask-parent {"question" " Which API should I preserve? "})))))

  (testing "returns coordinator failures as tool errors"
    (with-redefs [parent-coordinator/ask! (fn [& _]
                                            {:error true :message "Question limit reached."})]
      (is (match? {:error true
                   :contents [{:type :text :text "Question limit reached."}]}
                  (call-ask-parent {"question" "Another question?"}))))))

(deftest ask-parent-invalid-question-test
  (is (match? {:error true
               :contents [{:type :text :text #"INVALID_ARGS.*question"}]}
              (call-ask-parent {})))
  (is (match? {:error true
               :contents [{:type :text :text #"INVALID_ARGS.*question"}]}
              (call-ask-parent {"question" "  "}))))

(deftest ask-parent-enabled-test
  (let [enabled-fn (get-in f.tools.ask-parent/definitions ["ask_parent" :enabled-fn])
        active-runtime {:accepting?* (atom true)
                        :cancelled?* (atom false)}]
    (is (true? (enabled-fn {:chat-id "child"
                            :db {:chats {"child" {:subagent {:max-questions 2}
                                                    :parent-coordinator-id "coordinator"}}
                                 :parent-coordinators {"coordinator" active-runtime}}})))
    (is (false? (boolean (enabled-fn {:chat-id "child"
                                      :db {:chats {"child" {:subagent {:max-questions 0}
                                                              :parent-coordinator-id "coordinator"}}
                                           :parent-coordinators {"coordinator" active-runtime}}}))))
    (reset! (:accepting?* active-runtime) false)
    (is (false? (boolean (enabled-fn {:chat-id "child"
                                      :db {:chats {"child" {:subagent {:max-questions 2}
                                                              :parent-coordinator-id "coordinator"}}
                                           :parent-coordinators {"coordinator" active-runtime}}}))))))
