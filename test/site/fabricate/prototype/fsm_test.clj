(ns site.fabricate.prototype.fsm-test
  (:require [site.fabricate.prototype.fsm :refer :all]
            [site.fabricate.prototype.test-utils :refer [with-instrumentation]]
            [malli.core :as m]
            [malli.instrument :as mi]
            [clojure.test :as t]))

(t/use-fixtures :once with-instrumentation)

(def example-fsm
  {[:enum {:description "state 1"} 1] inc
   [:enum {:description "state 2"} 2] inc
   [:enum {:description "final state"} 3] identity})

(def fsm-additional-args
  {[:= {:description "state 1"} 1] +
   [:= {:description "state 2"} 2] (constantly 2)})

(t/deftest finite-schema-machines
  (t/testing "schema"
    (t/is (m/validate state-action-map example-fsm)
          "FSM should conform to spec"))

  (t/testing "advance"
    (t/is (= 3 (advance example-fsm 2)))
    (t/is (= 0 (advance example-fsm 0)))

    (t/is (= 2 (advance fsm-additional-args 1 1)))
    )

  (t/testing "complete"
    (t/is (= 3 (complete example-fsm 1)))
    (t/is (= 3 (complete example-fsm 2)))
    (t/is (= 0 (complete example-fsm 0)))

    (t/is (= 2 (complete fsm-additional-args 1 1)))))
