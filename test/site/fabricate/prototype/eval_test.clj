(ns site.fabricate.prototype.eval-test
  (:require [site.fabricate.prototype.eval :as prototype.eval]
            [site.fabricate.adorn :as adorn]
            [clojure.test :as t]))

(t/deftest evaluation
  (t/testing ": single exprs"
    (t/is (= 5
             (:value (prototype.eval/eval-form {:code "(+ 2 3)"
                                                :kindly/hide-code true
                                                :kindly/options {:hide-value
                                                                 false}
                                                :form (+ 2 3)}))))
    (t/is (= {:form           '(+ 2 3)
              :code           "(+ 2 3)"
              :value          5
              :kindly/hide-code true
              :kindly/options {:hide-value false}}
             (prototype.eval/eval-form {:code "(+ 2 3)"
                                        :kindly/hide-code true
                                        :kindly/options {:hide-value false}
                                        :form '(+ 2 3)})))))
