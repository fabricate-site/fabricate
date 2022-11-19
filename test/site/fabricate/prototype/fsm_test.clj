(ns site.fabricate.prototype.fsm-test
  (:require [site.fabricate.prototype.fsm :refer :all]
            [site.fabricate.prototype.test-utils :refer [with-instrumentation]]
            [malli.core :as m]
            [malli.instrument :as mi]
            [com.brunobonacci.mulog :as u]
            [clojure.test :as t]))

(def pub (u/start-publisher! {:type :console :pretty? true}))

(t/use-fixtures :once
  with-instrumentation
  (fn [f]
    (u/log ::start)
    (u/with-context {:type :test}
      (f))
    (pub)))

(def example-fsm
  ^{:fsm/name ::success}
  {[:= {:state/name ::success-1} 1] inc
   [:= {:state/name ::success-2} 2] inc
   [:= {:state/name ::success-3
        :state/description "final state"} 3] identity})

(def example-error-fsm
  ^{:fsm/name ::error}
  {[:= {:state/name ::error-initial} 1] inc
   [:= {:state/name ::error-second} 2] inc
   [:= {:state/name ::error-exception
        :state/description "intentional error state"} 3]
   (fn [_] (throw (Exception. "unknown error")))})

(def fsm-additional-args
  ^{:fsm/name ::varargs}
  {[:= {:state/name ::varargs-add
        :state/description "varargs test state"} 1] +
   [:= {:state/description ::varargs-constantly} 2] (constantly 2)})

(t/deftest finite-schema-machines
  (t/testing "schema"
    (t/is (m/validate state-action-map example-fsm)
          "FSM should conform to spec"))

  (t/testing "advance"
    (t/is (= 3 (advance example-fsm 2)))
    (t/is (= 0 (advance example-fsm 0)))

    (t/is (= {:state/value 0}
             (advance example-fsm {:state/value 0}))
          "FSMs should return the same results in 'debug' mode as normal operation")
    (t/is (= 3
             (:state/value (advance example-fsm {:state/value 2})))
          "FSMs should return the same results in 'debug' mode as normal operation")

    (t/is (= 3 (advance example-error-fsm 3))
          "FSM errors should be signaled with fallback to previous value")

    (t/is (= 2 (advance fsm-additional-args 1 1))))

  (t/testing "complete"
    (t/is (= 3 (complete example-fsm 1)))
    (t/is (= 3 (complete example-fsm 2)))
    (t/is (= 0 (complete example-fsm 0)))

    (t/is (= 2 (complete fsm-additional-args 1 1)))

    (t/is (= 3 (complete example-error-fsm 1)))
    (t/is (= 3 (complete example-error-fsm 2)))
    (t/is (= 3 (complete example-error-fsm 3)))))
