(ns site.fabricate.prototype.write-test
  (:require [site.fabricate.prototype.write :refer :all]
            [site.fabricate.prototype.read :as read]
            [site.fabricate.sketch :as sketch]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [malli.core :as m]
            [malli.util :as mu]
            [clojure.test :as t]))

(t/deftest page-fsm
  (t/testing "page metadata"
    (t/is (= {:namespace (symbol 'test-ns)
              :parsed-content [{:expr '(do (ns test-ns) nil)
                              :src "✳(ns test-ns)🔚"
                              :err nil
                              :result nil}]
              :output-file "./pages/test-file"
              :input-file "./content/test-file"}
             (populate-page-meta
              {:parsed-content [{:expr '(do (ns test-ns) nil)
                               :src "✳(ns test-ns)🔚"
                               :err nil
                               :result nil}]
               :input-file "./content/test-file"}
              {:output-dir "./pages"}))))

  (t/is (m/validate (-> populate-page-meta
                        var
                        meta
                        :malli/schema)
                    populate-page-meta)))

(t/deftest doc-rendering
  (t/testing "readme"
    (t/is (string?
           (-> "./README.md.fab"
               slurp
               read/parse
               read/eval-with-errors
               last)))

    (t/is (=  "./README.md.fab"
              (-> (sketch/advance-finite-schema-machine operations "./README.md.fab")
                  (get :input-file)
                  .getPath)))

    (t/is (= (slurp "./README.md.fab")
             (get
              (->> "./README.md.fab"
                   (sketch/advance-finite-schema-machine operations)
                   (sketch/advance-finite-schema-machine operations))
              :unparsed-content)))


    (comment

      (m/validate (-> sketch/page-metadata-schema
                      (mu/dissoc :output-file)
                      (mu/dissoc :title)
                      (mu/dissoc :namespace)
                      (mu/dissoc :page-style)
                      (mu/dissoc :parsed-content)
                      (mu/dissoc :hiccup-content)
                      (mu/dissoc :rendered-content))

                  (->> "./README.md.fab"
                       (sketch/advance-finite-schema-machine operations)
                       (sketch/advance-finite-schema-machine operations))))

    (t/is (set/subset?
           #{:parsed-content :namespace}
           (->> "./README.md.fab"
                (sketch/advance-finite-schema-machine operations)
                (sketch/advance-finite-schema-machine operations)
                (sketch/advance-finite-schema-machine operations)
                keys
                (into #{}))))

    (t/is (contains?
           (->> "./README.md.fab"
                (sketch/advance-finite-schema-machine operations)
                (sketch/advance-finite-schema-machine operations)
                (sketch/advance-finite-schema-machine operations)
                (sketch/advance-finite-schema-machine operations))
           :evaluated-content))

    #_(t/is (contains?
             (->> "./README.md.fab"
                  (sketch/advance-finite-schema-machine operations)
                  (sketch/advance-finite-schema-machine operations)
                  (sketch/advance-finite-schema-machine operations)
                  (sketch/advance-finite-schema-machine operations)
                  (sketch/advance-finite-schema-machine operations)
                  :value)
             :rendered-content))))
