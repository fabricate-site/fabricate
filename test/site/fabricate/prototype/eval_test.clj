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
                                                     :form '(+ 2 3)}))))
    (t/is (match? {:form           '(+ 2 3)
                   :code           "(+ 2 3)"
                   :value          5
                   :kindly/hide-code true
                   :kindly/options {:hide-value false}}
                  (prototype.eval/eval-form {:code "(+ 2 3)"
                                             :kindly/hide-code true
                                             :kindly/options {:hide-value false}
                                             :form '(+ 2 3)})))
    (t/is
     (match?
      {:form '(inc "a")
       :code "(inc \"a\")"
       :error
       {:cause
        "class java.lang.String cannot be cast to class java.lang.Number (java.lang.String and java.lang.Number are in module java.base of loader 'bootstrap')"}}
      (prototype.eval/eval-form {:form '(inc "a") :code "(inc \"a\")"})))
    (t/testing "custom namespaces"
      (let [test-ns (create-ns 'eval-test-ns)]
        (t/is (match?
               {:form '(def test-var {:a :b}) :ns test-ns :meta {:ns test-ns}}
               (prototype.eval/eval-form {:form '(def test-var {:a :b})
                                          :ns   test-ns})))))
    (t/testing "metadata after evaluation"
      ;; TODO: should Fabricate add its own metadata to trace provenance?
      (let [test-ns        (create-ns 'site.fabricate.prototype.eval.ephemeral)
            var-form       {:code "(def my-test-var \"test value\")"
                            :form '(def my-test-var "test value")
                            :ns   test-ns}
            evaluated-form (prototype.eval/eval-form var-form)
            metadata-form  (prototype.eval/eval-form
                            {:form '[^{:key :value} [1]]
                             :code "[^{:key :value} [1]]"
                             :ns   test-ns})]
        (println (meta (:value evaluated-form)))
        (t/is (match? (:ns var-form) (:ns (meta (:value evaluated-form)))))
        (t/is (match? {:key :value} (meta (first (:value metadata-form)))))))))
