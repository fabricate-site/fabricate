(ns site.fabricate.prototype.schema-test
  (:require [site.fabricate.prototype.schema :refer :all]
            [site.fabricate.prototype.html]
            [site.fabricate.prototype.hiccup]
            [site.fabricate.prototype.read]
            [site.fabricate.prototype.read.grammar]
            [site.fabricate.prototype.test-utils :as test-utils]
            [malli.core :as m]
            [clojure.test :as t]))

(t/deftest schema-utils
  (t/testing "malli schema predicates"
    (t/is (malli? :any))
    (t/is (malli? (m/schema :any)))
    (t/is (malli? [:cat [:* :string] :int]))
    (t/is (has-reqd? [:map [:a {:optional true} :string] [:b :string]]))
    (t/is (has-reqd? [:map [:a :string]]))
    (t/is (false? (has-reqd? [:map [:a {:optional true} :string]
                              [:b {:optional true} :string]]))))
  (t/testing "malli schema transforms"
    (t/is (malli? (subschema
                   [:schema
                    {:registry
                     {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                      ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
                    ::ping]
                   ::pong)))
    (t/is (malli? (subschema
                   (m/schema
                    [:schema
                     {:registry
                      {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                       ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
                     ::ping])
                   ::pong)))
    (t/is
     (thrown? clojure.lang.ExceptionInfo
              (subschema
               (m/schema [:schema
                          {:registry
                           {::ping [:maybe [:tuple [:= "ping"] [:ref ::pong]]]
                            ::pong [:maybe [:tuple [:= "pong"] [:ref ::ping]]]}}
                          ::ping])
               ::pung))
     "Attempting to construct a subschema with an invalid key should throw an exception.")
    (t/is (malli? (unify [:int :string])))
    (t/is (= [0 23] (m/parse (unify [:int :string]) 23)))
    (t/is (= [1 "this"] (m/parse (unify [:int :string]) "this"))))
  (t/testing "general purpose schemas + validators"
    (let [exception-map (try (#(throw (Exception. "error")))
                             (catch Exception e (Throwable->map e)))]
      (t/is (valid-schema? throwable-map-schema exception-map))
      (t/is (throwable-map? exception-map)))))

(defn has-schema?
  [v]
  (let [var-schema (:malli/schema (meta v))
        val-schema (:malli/schema (meta (var-get v)))]
    (or (malli? var-schema)
        (malli? val-schema)
        (malli? (var-get v))
        (not (fn? (var-get v))))))

(comment
  (has-schema? #'site.fabricate.prototype.html/element-flat-explainer))

(defn test-ns-schemas
  [nmspc]
  (for [[sym value] (ns-publics nmspc)]
    (t/testing (str ": " value)
      {:namespace nmspc :var value :schema? (has-schema? value)})))

(defmethod t/assert-expr 'covered?
  [msg form]
  `(let [ns-results# ~(nth form 1)
         nmspc#      (:namespace (first ns-results#))
         coverage#   (* 1.0
                        (/ (count (filter :schema? ns-results#))
                           (count ns-results#)))
         result#     (= 1.0 coverage#)]
     (t/do-report
      {:type     (if result# :pass :fail)
       :message  (str ~msg (format "%.2f%% coverage" (* coverage# 100)))
       :expected (format "namespace %s has full schema coverage" nmspc#)
       :actual   (if (not result#)
                   (str "functions without a schema:\n"
                        (->> ns-results#
                             (filter #(false? (:schema? %)))
                             (map :var)
                             (clojure.string/join " \n")))
                   result#)})
     result#))


(t/deftest schema-coverage
  (doseq [nmspc '(site.fabricate.prototype.html
                  site.fabricate.prototype.read
                  site.fabricate.prototype.read.grammar
                  site.fabricate.prototype.hiccup
                  site.fabricate.prototype.schema
                  site.fabricate.api)]
    (t/testing (str "coverage for namespace " nmspc)
      (let [ns-results (test-ns-schemas nmspc)] (t/is (covered? ns-results))))))
