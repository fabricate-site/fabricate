(ns site.fabricate.prototype.fsm-test
  (:require [site.fabricate.prototype.fsm :refer :all]
            [malli.core :as m]
            [clojure.test :as t]))

(def example-fsm
  {[:enum 1] inc
   [:enum 2] inc
   [:enum 3] identity})

(t/deftest finite-schema-machines
  (t/testing "schema"
    (t/is (m/validate state-action-map example-fsm)
          "FSM should conform to spec"))

  (t/testing "advance"
    (t/is (= 3 (advance example-fsm 2))))

  (t/testing "complete"
    (t/is (= 3 (complete example-fsm 1)))
    (t/is (= 3 (complete example-fsm 2)))))
