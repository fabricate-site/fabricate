(ns site.fabricate.prototype.write-test
  (:require [site.fabricate.prototype.write :refer :all]
            [site.fabricate.prototype.read :as read]
            [site.fabricate.prototype.fsm :as fsm]
            [site.fabricate.sketch :as sketch]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [malli.core :as m]
            [malli.util :as mu]
            [http.server :as server]
            [clojure.test :as t]
            [babashka.curl :as curl]))

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
                   "./README.md.fab"
                   initial-state)
                  (get :site.fabricate.file/input-file)
                  .getPath)))

    (let [evaluated
          (:site.fabricate.page/evaluated-content
           (fsm/complete
            (dissoc operations
                    markdown-state
                    rendered-state)
            initial-state
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
               "./README.md.fab"
               initial-state)
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
       "./README.md.fab"
       initial-state)))

    (let [output
          (fsm/complete
           (select-keys operations [input-state file-state read-state])
           "./README.md.fab"
           initial-state)
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
            :site.fabricate.page/unparsed-content "✳=(with-meta [:div \"text\"] {:page/title \"text\"})🔚"}
           initial-state)]
      (t/is
       (and (:site.fabricate.page/metadata meta-post)
            (or (contains? (:site.fabricate.page/metadata meta-post) :title)))))

    (let [sample-error
          (->> {:site.fabricate.file/input-file (io/file "content/test/some-file.txt.fab")
                :site.fabricate.file/filename "content/test/some-file.txt.fab"
                :site.fabricate.page/unparsed-content "✳=(unbound-fn nil)🔚"}
               (#(fsm/complete (dissoc operations
                                       rendered-state
                                       html-state
                                       markdown-state)
                               %
                               initial-state))
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
                         "./README.md.fab"
                         initial-state)
           :site.fabricate.page/evaluated-content))

    (t/is
     (m/validate rendered-state
                 (fsm/complete (dissoc operations
                                       html-state
                                       rendered-state)
                               "./README.md.fab"
                               initial-state)))))

(t/deftest existing-pages
  (t/testing "existing pages"
    (let [operations
          (assoc operations
                 rendered-state
                 (fn [{:keys [site.fabricate.page/rendered-content]
                       :as page-data}
                      settings]
                   (do
                     (t/is (any? rendered-content))
                     page-data)))]
      (doseq [page-path (get-template-files "./pages" ".fab")]
        (println "testing" page-path)
        (fsm/complete operations page-path initial-state)))))

(comment
  (:status (curl/get "https://respatialized.github.io/"))

  (let [srv (server/start {:port 9800})]
    (server/stop srv))

  )

(defn delete-directory-recursive
  "Recursively delete a directory."
  [^java.io.File file]
  ;; when `file` is a directory, list its entries and call this
  ;; function with each entry. can't `recur` here as it's not a tail
  ;; position, sadly. could cause a stack overflow for many entries?
  ;; thanks to @nikolavojicic for the idea to use `run!` instead of
  ;; `doseq` :)
  (when (.isDirectory file)
    (run! delete-directory-recursive (.listFiles file)))
  ;; delete the file or directory. if it it's a file, it's easily
  ;; deletable. if it's a directory, we already have deleted all its
  ;; contents with the code above (remember?)
  (io/delete-file file))

(t/deftest application-state
  (t/testing "ability to manage server state using send and draft!"

    (let [test-config
          (-> initial-state
              (assoc-in [:site.fabricate/settings
                         :site.fabricate.file/input-dir]
                        "./test-resources/fab/inputs/")
              (assoc-in [:site.fabricate/settings
                         :site.fabricate.file/output-dir]
                        "./test-resources/fab/outputs/")
              (assoc-in [:site.fabricate/settings
                         :site.fabricate.server/config]
                        {:dir "./test-resources/fab/outputs/"
                         :port 9223}))
          url "http://localhost:9223"
          test-fabricate-str
          "✳(ns site.fabricate.prototype.write-test.post)🔚
✳(def metadata {:title \"an example post\"})🔚
Some plaintext
✳=[:h1 (:title metadata)]🔚
Some more text"
          extra-content-str "\n\n Three plus four is: ✳=[:strong (+ 3 4)]🔚"
          test-state (agent test-config)]
      (println "starting up")
      #_(try (restart-agent test-state test-config)
           (catch Exception e nil))
      (io/make-parents "./test-resources/fab/outputs/.nothing")
      (io/make-parents "./test-resources/fab/inputs/.nothing")
      (send test-state draft!)
      (Thread/sleep 1500)
      (t/is (#{200 304} (:status (curl/get url)))
            "Server should start via agent")
      (spit "./test-resources/fab/inputs/test-file.html.fab"
            test-fabricate-str)
      (Thread/sleep 250)
      (let [response (curl/get url)]
        (t/is (re-find #"test\-file\.html" (:body response))
              "File should display in list of files after rendering"))
      (t/is (#{200 304} (:status (curl/get (str url "/test-file.html"))))
            "File should be visible on server")
      (Thread/sleep 250)

      (spit "./test-resources/fab/inputs/test-file.html.fab"
            extra-content-str
            :append true)

      (Thread/sleep 250)
      (t/is (re-find #"four" (:body (curl/get (str url "/test-file.html"))))
            "File should have contents updated by filewatcher")

      (send test-state stop!)
      (delete-directory-recursive (io/file "test-resources/fab"))

      (Thread/sleep 500)
      (t/is (nil? (:status (curl/get url {:throw false})))
            "Server should shutdown via agent"))))
