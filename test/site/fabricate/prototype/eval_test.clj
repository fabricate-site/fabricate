(ns site.fabricate.prototype.eval-test
  (:require [site.fabricate.prototype.eval :as prototype.eval]
            [site.fabricate.adorn :as adorn]
            [matcher-combinators.test]
            [matcher-combinators.matchers :as matchers]
            [clojure.test :as t]))

(t/deftest evaluation
  (t/testing ": single exprs"
    (t/is (match? 5
                  (:value (prototype.eval/eval-form {:code "(+ 2 3)"
                                                     :kindly/hide-code true
                                                     :kindly/options
                                                     {:hide-value false}
                                                     :form (+ 2 3)}))))
    (t/is (match? {:form           '(+ 2 3)
                   :code           "(+ 2 3)"
                   :value          5
                   :kindly/hide-code true
                   :kindly/options {:hide-value false}}
                  (prototype.eval/eval-form {:code "(+ 2 3)"
                                             :kindly/hide-code true
                                             :kindly/options {:hide-value false}
                                             :form '(+ 2 3)})))
    (t/testing "custom namespaces"
      (let [test-ns (create-ns 'eval-test-ns)]
        (t/is (match?
               {:form '(def test-var {:a :b}) :ns test-ns :meta {:ns test-ns}}
               (prototype.eval/eval-form {:form '(def test-var {:a :b})
                                          :ns   test-ns})))))))
