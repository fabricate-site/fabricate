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
  (t/testing "output path fn"
    (t/is (= "./docs/test-file.txt"
             (get-output-filename "./pages/test-file.txt"
                                  "./pages"
                                  "./docs")))))

(t/deftest page-fsm
  (t/testing "page metadata"
    (let [ex-file (io/file "./content/test-file.txt.fab")]
      (t/is (= {:site.fabricate.page/namespace (symbol 'test-ns)
                :site.fabricate.page/parsed-content
                [{:exec '(ns test-ns)
                  :src "(ns test-ns)"}]
                :site.fabricate.file/filename "content/test-file"
                :site.fabricate.file/output-extension "txt"
                :site.fabricate.file/template-suffix ".fab"
                :site.fabricate.file/output-file "./pages/test-file.txt"
                :site.fabricate.file/input-file ex-file}
               (populate-page-meta
                {:site.fabricate.page/parsed-content [{:exec '(ns test-ns)
                                                       :src "(ns test-ns)"}]
                 :site.fabricate.file/input-file ex-file}
                {:site.fabricate.file/output-dir "./pages"
                 :site.fabricate.file/input-dir "./content"}))))

    (t/is (m/validate (-> populate-page-meta
                          var
                          meta
                          :malli/schema)
                      populate-page-meta))))

(t/deftest doc-rendering
  (t/testing "readme"
    (t/is (string?
           (-> "./README.md.fab"
               slurp
               read/parse
               (read/eval-all true 'site.fabricate.docs.readme)
               last)))

    (t/is (=  "./README.md.fab"
              (-> (fsm/complete
                   (select-keys operations [input-state])
                   "./README.md.fab")
                  (get :site.fabricate.file/input-file)
                  .getPath)))

    (let [evaluated
          (:site.fabricate.page/evaluated-content
           (fsm/complete
            (dissoc operations
                    markdown-state
                    rendered-state)
            "./README.md.fab"))
          errors
          (filter #(m/validate read/error-form-schema %)
                  (tree-seq vector? rest evaluated))]
      (t/is (= (count errors) 0) "The README should not have errors in it")
      (when (not= (count errors) 0)
        (println errors)))

    (t/is (= (slurp "./README.md.fab")
             (get
              (fsm/complete
               (select-keys operations [input-state file-state])
               "./README.md.fab")
              :site.fabricate.page/unparsed-content)))

    (t/is
     (m/validate
      (-> page-metadata-schema
          (mu/dissoc :site.fabricate.file/output-file)
          (mu/dissoc :site.fabricate.page/title)
          (mu/dissoc :site.fabricate.page/namespace)
          (mu/dissoc :site.fabricate.page/stylesheet)
          (mu/dissoc :site.fabricate.page/parsed-content)
          (mu/dissoc :site.fabricate.page/hiccup-content)
          (mu/dissoc :site.fabricate.page/rendered-content))
      (fsm/complete
       (select-keys operations [input-state file-state])
       "./README.md.fab")))

    (let [output
          (fsm/complete
           (select-keys operations [input-state file-state read-state])
           "./README.md.fab")
          out-keys  (->> output keys (into #{}))
          out-file (:site.fabricate.file/output-file output)]
      (println out-keys)
      (println out-file)
      (t/is (set/subset?
             #{:site.fabricate.page/parsed-content
               :site.fabricate.page/namespace}
             out-keys)
            "Metadata should be properly populated")
      (t/is (= "./README.md" out-file)))

    (t/is (= "public/test/some-file.txt"
             (:site.fabricate.file/output-file
              (populate-page-meta {:site.fabricate.file/input-file (io/file "content/test/some-file.txt.fab")
                                   :site.fabricate.file/output-file "public/test/some-file.txt"}
                                  default-site-settings))))

    (let [meta-post
          (fsm/complete
           (dissoc operations
                   rendered-state
                   html-state
                   markdown-state)
           {:site.fabricate.file/input-file (io/file "content/test/some-file.txt.fab")
            :site.fabricate.file/filename "content/test/some-file.txt.fab"
            :site.fabricate.page/unparsed-content "✳=(with-meta [:div \"text\"] {:page/title \"text\"})🔚"})]
      (t/is
       (and (:site.fabricate.page/metadata meta-post)
            (or (contains? (:site.fabricate.page/metadata meta-post) :title)))))

    (let [sample-error
          (->> {:site.fabricate.file/input-file (io/file "content/test/some-file.txt.fab")
                :site.fabricate.file/filename "content/test/some-file.txt.fab"
                :site.fabricate.page/unparsed-content "✳=(unbound-fn nil)🔚"}
               (fsm/complete (dissoc operations
                                     rendered-state
                                     html-state
                                     markdown-state))
               :site.fabricate.page/evaluated-content
               first)]
      (t/is
       (and (= :div (first sample-error))
            (= [:h6 "Error"] (second sample-error))
            (some? (second (nth (nth sample-error 2) 8))))
       "Errors should be correctly surfaced in output"))

    (t/is (contains?
           (fsm/complete (dissoc operations
                                 markdown-state
                                 html-state
                                 rendered-state)
                         "./README.md.fab")
           :site.fabricate.page/evaluated-content))

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
