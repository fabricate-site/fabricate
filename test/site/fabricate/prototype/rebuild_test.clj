(ns site.fabricate.prototype.rebuild-test
  (:require [clojure.test :as t]
            [site.fabricate.api :as api]
            [site.fabricate.prototype.rebuild :refer :all])
  (:import [java.util.concurrent LinkedBlockingQueue]))

(def test-status (atom {:queue :waiting}))

(def test-queue)

(def ^:dynamic *test-process* nil)

(defn test-fixture
  [f]
  (binding [*test-process* (future (while true
                                     (swap! test-status update-state)))]
    (f)
    (future-cancel *test-process*)
    (swap! test-status assoc :queue :stopped)
    (swap! test-status update-state)))

(t/use-fixtures :once test-fixture)

(t/deftest actions
  (let [test-events (atom [])]
    ;; using a sequence of "expected actions" opens up the door to more
    ;; expressive property checks in the future, such as:
    ;; - 'the size of the list of actions performed cannot increase while
    ;; the statue is :terminated or :waiting'
    ;; - 'each action in a sequence of actions to be performed will occur
    ;; exactly once or never'
    ;; - etc
    (t/testing "event loop state machine:"
      (swap! test-status assoc :queue :running)
      (t/testing "basic actions"
        (let [_output (.put action-queue
                            #(swap! test-events conj :test-action))]
          (Thread/sleep 25) ;; is there a more principled way of doing
          ;; this?
          (t/is (= [:test-action] @test-events))
          (t/is (= :running (:queue @test-status))
                "Regular actions should run without errors")))
      (t/testing "exceptions"
        (let [_output (.put action-queue
                            #(do (swap! test-events conj :test-error)
                                 (throw (ex-info "manual exception"
                                                 {:context :test}))))]
          (Thread/sleep 25)
          (t/is (= :waiting (:queue @test-status))
                "Exceptions should pause event loop")
          (t/is (= :test-error (peek @test-events))
                "Error should be present in sequence of actions")
          (.put action-queue #(do (swap! test-events conj :test-pause)))
          (t/is (not (empty? (seq action-queue)))
                "Actions enqueued while waiting should remain on the queue")
          (swap! test-status assoc :queue :running)
          (Thread/sleep 25)
          (t/is
           (= :test-pause (peek @test-events))
           "Actions enqueued while waiting should be performed after events resume")
          (t/is (false? (future-done? *test-process*))
                "Process should be running at the end of the tests"))))))

(def valid-queue-state-updates
  #{[:running :running] [:running :waiting] [:waiting :running]
    [:running :stopped] [:waiting :waiting] [:stopped :terminated]
    [:terminated :terminated] [:terminated :waiting] [:terminated :running]})

(defn check-state-updates
  [_k _ref {old-queue-state :queue} {new-queue-state :queue}]
  (if (empty? t/*testing-vars*)
    (assert (contains? valid-queue-state-updates
                       [old-queue-state new-queue-state])
            (format "state updates should be fully enumerated for [%s, %s]"
                    old-queue-state
                    new-queue-state))
    (t/is (contains? valid-queue-state-updates
                     [old-queue-state new-queue-state])
          (format "state updates should be fully enumerated for [%s, %s]"
                  old-queue-state
                  new-queue-state))))

(defn enable-dev-mode!
  [state-atom]
  (add-watch state-atom :queue-state check-state-updates))

(comment
  (println t/*testing-vars*)
  (enable-dev-mode! test-status)
  (remove-watch test-status :queue-state))
