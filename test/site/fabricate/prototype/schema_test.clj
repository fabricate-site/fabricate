(ns site.fabricate.prototype.schema-test
  (:require [site.fabricate.prototype.schema :refer [malli?]]
            [site.fabricate.prototype.html]
            [site.fabricate.prototype.page]
            [site.fabricate.prototype.fsm]
            [site.fabricate.prototype.read]
            [site.fabricate.prototype.read.grammar]
            [site.fabricate.prototype.write]
            [clojure.test :as t]))


(comment (ns-publics 'site.fabricate.prototype.fsm))

(defn test-ns-schemas [nmspc]
  (for [[sym value] (ns-publics nmspc)]
    (do
      (t/testing (str ": " value))
      (let [schema (:malli/schema (meta value))
            schema? (or (and (fn? (var-get value)) (some? schema))
                        (malli? (var-get value))
                        (not (fn? (var-get value))))]
        {:namespace nmspc :var value :schema? schema?}))))

(defmethod t/assert-expr 'covered? [msg form]
  `(let [ns-results# ~(nth form 1)
         nmspc# (:namespace (first ns-results#))
         coverage# (* 1.0 (/ (count (filter :schema? ns-results#))
                             (count ns-results#)))
         result# (= 1.0 coverage#)]
     (t/do-report
      {:type (if result# :pass :fail)
       :message (str ~msg (format "%.2f%% coverage" (* coverage# 100)))
       :expected (format "namespace %s has full schema coverage"
                         nmspc#)
       :actual  (if (not result#)
                  (str "uninstrumented symbols:\n"
                       (->> ns-results#
                            (filter #(false? (:schema? %)))
                            (map :var)
                            (clojure.string/join "\n")))
                  result#)})
     result#))



(comment
  (malli? (var-get #'site.fabricate.prototype.fsm/state-action-map))

  (test-ns-schemas 'site.fabricate.prototype.fsm))

(t/deftest schema-coverage
  (doseq [nmspc '(site.fabricate.prototype.fsm
                  site.fabricate.prototype.html
                  site.fabricate.prototype.read
                  site.fabricate.prototype.page
                  site.fabricate.prototype.read.grammar
                  site.fabricate.prototype.write)]
    (t/testing (str "coverage for namespace " nmspc)
      (let [ns-results (test-ns-schemas nmspc)]
        (t/is (covered? ns-results))))))
