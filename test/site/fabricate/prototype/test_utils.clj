(ns site.fabricate.prototype.test-utils
  (:require [malli.core :as m]
            [malli.error :as me]
            [malli.instrument :as mi]
            [clojure.test :as t]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))

(defn cli-test? [] (= "cli.test" (System/getProperty "clojure.context")))



(defn check-schema
  ([schema value msg]
   (t/is (m/validate schema value)
         (or (not-empty (str (when msg (str msg "\n"))
                             (me/humanize (m/explain schema value))))
             "value conforms to schema")))
  ([schema value] (check-schema schema value nil)))

(defn with-instrumentation
  "Test fixture enabling malli instrumentation to check function conformance to malli schemas"
  [f]
  (mi/collect!)
  (mi/instrument! {:report
                   (fn report [key {:keys [output value] :as error-data}]
                     (when output
                       (check-schema
                        output
                        value
                        "Instrumented function didn't conform to schema")))})
  (f)
  (mi/unstrument!))

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
