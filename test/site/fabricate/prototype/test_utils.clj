(ns site.fabricate.prototype.test-utils
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.instrument :as mi]
            [clojure.test :as t]
            [clojure.pprint :as pprint]))

(defn with-instrumentation
  [f]
  (mi/collect!)
  (mi/instrument!)
  (f)
  (mi/unstrument!))

(defmethod t/assert-expr 'valid-schema?
  [msg form]
  `(let [schema#      ~(nth form 1)
         form#        (m/form schema#)
         data#        ~(nth form 2)
         result#      (m/validate schema# data#)
         schema-name# (last form#)]
     (t/do-report {:type     (if result# :pass :fail)
                   :message  ~msg
                   :expected (str (with-out-str (pprint/pprint data#))
                                  " conforms to schema for "
                                  schema-name#)
                   :actual   (if (not result#)
                               (me/humanize (m/explain schema# data#))
                               result#)})
     result#))

(defn gather-test-meta
  "Obtain information about the current test as a map"
  []
  (let [t-ctx  t/*testing-contexts*
        t-vars t/*testing-vars*]
    (into {}
          [(when (seq t-ctx) [:clojure.test/contexts t-ctx])
           (when (seq t-vars) [:clojure.test/vars t-vars])])))

(t/deftest meta-util
  (t/testing "metadata"
    (let [ctx (gather-test-meta)] ; meta, indeed
      (t/is (= (list "metadata") (:clojure.test/contexts ctx)))
      (t/is (= (list #'meta-util) (:clojure.test/vars ctx))))))
