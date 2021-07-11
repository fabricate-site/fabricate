(ns site.fabricate.prototype.write-test
  (:require [site.fabricate.prototype.write :refer :all]
            [site.fabricate.prototype.read :as read]
            [site.fabricate.sketch :as sketch]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [malli.core :as m]
            [malli.util :as mu]
            [clojure.test :as t]))

(t/deftest file-utils
  #_(t/testing "suffix regex"
    (t/is (re-matches template-suffix-regex "test-file.txt.fab")))
  (t/testing "output path fn"
    (t/is (= "./pages/test-file.txt"
             (get-output-filename "./pages/test-file.txt.fab")))))

(t/deftest page-fsm
  (t/testing "page metadata"
    (let [ex-file (io/file "./content/test-file.txt.fab")]
      (t/is (= {:namespace (symbol 'test-ns)
                :parsed-content [{:expr '(do (ns test-ns) nil)
                                  :src "✳(ns test-ns)🔚"
                                  :err nil
                                  :result nil}]
                :filename "content/test-file"
                :file-extension "txt"
                :fabricate/suffix ".fab"
                :output-file "./pages/test-file.txt"
                :input-file ex-file}
               (populate-page-meta
                {:parsed-content [{:expr '(do (ns test-ns) nil)
                                   :src "✳(ns test-ns)🔚"
                                   :err nil
                                   :result nil}]
                 :input-file ex-file}
                {:output-dir "./pages"})))))

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

    (t/is
     (let [output (->> "./README.md.fab"
                       (sketch/advance-finite-schema-machine operations)
                       (sketch/advance-finite-schema-machine operations)
                       (sketch/advance-finite-schema-machine operations))]
       (and (set/subset?
             #{:parsed-content :namespace}
             (->> output keys (into #{})))
            (= "./README.md" (:output-file output))))

     "Metadata should be properly populated")



    (t/is (contains?
           (->> "./README.md.fab"
                (sketch/advance-finite-schema-machine operations)
                (sketch/advance-finite-schema-machine operations)
                (sketch/advance-finite-schema-machine operations)
                (sketch/advance-finite-schema-machine operations))
           :evaluated-content))

    (t/is
     (->> "./README.md.fab"
          (sketch/advance-finite-schema-machine operations)
          (sketch/advance-finite-schema-machine operations)
          (sketch/advance-finite-schema-machine operations)
          (sketch/advance-finite-schema-machine operations)
          (sketch/advance-finite-schema-machine operations)
          (m/validate rendered-state)))

    #_(t/is (contains?
             (->> "./README.md.fab"
                  (sketch/advance-finite-schema-machine operations)
                  (sketch/advance-finite-schema-machine operations)
                  (sketch/advance-finite-schema-machine operations)
                  (sketch/advance-finite-schema-machine operations)
                  (sketch/advance-finite-schema-machine operations)
                  :value)
             :rendered-content))))
