(ns site.fabricate.prototype.write-test
  (:require [site.fabricate.prototype.write :refer :all]
            [site.fabricate.prototype.read :as read]
            [site.fabricate.prototype.fsm :as fsm]
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
    (t/is (= "./docs/test-file.txt"
             (get-output-filename "./pages/test-file.txt"
                                  "./pages"
                                  "./docs")))))

(t/deftest page-fsm
  (t/testing "page metadata"
    (let [ex-file (io/file "./content/test-file.txt.fab")]
      (t/is (= {:namespace (symbol 'test-ns)
                :parsed-content [{:exec '(ns test-ns)
                                  :src "(ns test-ns)"}]
                :filename "content/test-file"
                :file-extension "txt"
                :fabricate/suffix ".fab"
                :output-file "./pages/test-file.txt"
                :input-file ex-file}
               (populate-page-meta
                {:parsed-content [{:exec '(ns test-ns)
                                   :src "(ns test-ns)"}]
                 :input-file (io/file "./content/test-file.txt.fab") }
                {:output-dir "./pages"
                 :input-dir "./content"})))))

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
              (-> (fsm/complete
                   (select-keys operations [input-state])
                   "./README.md.fab")
                  (get :input-file)
                  .getPath)))

    (t/is (= (slurp "./README.md.fab")
             (get
              (fsm/complete
               (select-keys operations [input-state file-state])
               "./README.md.fab")
              :unparsed-content)))

    (t/is
     (m/validate
      (-> sketch/page-metadata-schema
          (mu/dissoc :output-file)
          (mu/dissoc :title)
          (mu/dissoc :namespace)
          (mu/dissoc :page-style)
          (mu/dissoc :parsed-content)
          (mu/dissoc :hiccup-content)
          (mu/dissoc :rendered-content))
      (fsm/complete
       (select-keys operations [input-state file-state])
       "./README.md.fab")))

    (let [output
          (fsm/complete
           (select-keys operations [input-state file-state read-state])
           "./README.md.fab")
          out-keys  (->> output keys (into #{}))
          out-file (:output-file output)]
      (println out-keys)
      (println out-file)
      (t/is
       (and (set/subset?
             #{:parsed-content :namespace}
             out-keys)
            (= "./README.md" out-file))

       "Metadata should be properly populated"))

    (t/is (= "public/test/some-file.txt"
             (:output-file
              (populate-page-meta {:input-file (io/file "content/test/some-file.txt.fab")
                                   :output-file "public/test/some-file.txt"}
                                  default-site-settings))))




    (t/is (contains?
           (fsm/complete (dissoc operations
                                 markdown-state
                                 html-state
                                 rendered-state)
                         "./README.md.fab")
           :evaluated-content))

    (t/is
     (m/validate rendered-state
                 (fsm/complete (dissoc operations
                                       html-state
                                       rendered-state)
                               "./README.md.fab"))))

  (t/testing "existing pages"
    (let [operations
          (assoc operations
                 rendered-state
                 (fn [{:keys [rendered-content]
                       :as page-data}]
                   (do
                     (t/is (any? rendered-content))
                     page-data)))]
      (doseq [page-path (get-template-files "./pages" ".fab")]
        (println "testing" page-path)
        (fsm/complete operations page-path)))))

(comment
  (let [operations
        (dissoc operations
                rendered-state
                html-state)]

    (evaluated->hiccup
     (fsm/complete operations
                   "./pages/fabricate.html.fab"))))
