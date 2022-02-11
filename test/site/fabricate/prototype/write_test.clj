(ns site.fabricate.prototype.write-test
  (:require [site.fabricate.prototype.write :as write :refer :all]
            [site.fabricate.prototype.read :as read]
            [site.fabricate.prototype.fsm :as fsm]
            [site.fabricate.prototype.test-utils :refer [with-instrumentation]]
            [site.fabricate.sketch :as sketch]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.data]
            [malli.core :as m]
            [malli.util :as mu]
            [http.server :as server]
            [clojure.test :as t]
            [babashka.curl :as curl])
  (:import  [java.util.concurrent Executors]))

(declare test-state)

(t/use-fixtures :once
  (fn [f]
    (let [prior-exec clojure.lang.Agent/soloExecutor
          test-exec (Executors/newWorkStealingPool 20)]
      (set-agent-send-executor! test-exec)
      (set-agent-send-off-executor! test-exec)
      (def test-state (agent initial-state))
      (with-instrumentation f)
      (println "stopping")
      (send test-state stop!)
      (set-agent-send-executor! prior-exec)
      (set-agent-send-off-executor! prior-exec))))

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
                   (select-keys default-operations [input-state])
                   "./README.md.fab"
                   initial-state)
                  (get :site.fabricate.file/input-file)
                  .getPath)))

    (let [evaluated
          (:site.fabricate.page/evaluated-content
           (fsm/complete
            (dissoc default-operations
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
               (select-keys default-operations [input-state file-state])
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
       (select-keys default-operations [input-state file-state])
       "./README.md.fab"
       initial-state)))

    (let [output
          (fsm/complete
           (select-keys default-operations [input-state file-state read-state])
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
           (dissoc default-operations
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
               (#(fsm/complete (dissoc default-operations
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
           (fsm/complete (dissoc default-operations
                                 markdown-state
                                 html-state
                                 rendered-state)
                         "./README.md.fab"
                         initial-state)
           :site.fabricate.page/evaluated-content))

    (t/is
     (m/validate rendered-state
                 (fsm/complete (dissoc default-operations
                                       html-state
                                       rendered-state)
                               "./README.md.fab"
                               initial-state)))))

(t/deftest existing-pages
  (t/testing "existing pages"
    (let [default-operations
          (assoc default-operations
                 rendered-state
                 (fn [{:keys [site.fabricate.page/rendered-content]
                       :as page-data}
                      settings]
                   (do
                     (t/is (any? rendered-content))
                     page-data)))]
      (doseq [page-path (get-template-files "./pages" ".fab")]
        (println "testing" page-path)
        (fsm/complete default-operations page-path initial-state)))))

(comment
  (:status (curl/get "https://respatialized.github.io/" {:throw false}))

  (let [srv (server/start {:port 9800})]
    (server/stop srv)))

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

(def test-fabricate-str
  "✳(ns site.fabricate.prototype.write-test.post)🔚
✳(def metadata {:title \"an example post\"})🔚
Some plaintext
✳=[:h1 (:title metadata)]🔚
Some more text")

(def extra-content-str "\n\n Three plus four is: ✳=[:strong (+ 3 4)]🔚")

(def test-config
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
                 :port 9223})))

(t/deftest application-state
  (println "creating test dir")
  (io/make-parents "./test-resources/fab/outputs/.nothing")
  (io/make-parents "./test-resources/fab/inputs/.nothing")

  (t/testing "rerender fn"
    (let [f (do (spit "./test-resources/fab/inputs/test-file.html.fab"
                      test-fabricate-str)
                "./test-resources/fab/inputs/test-file.html.fab")
          res (rerender test-config
                        {:file (io/file f) :count 1 :action :create})
          rendered-str (get-in res [:site.fabricate/pages
                                    "test-resources/fab/inputs/test-file.html.fab"
                                    :site.fabricate.page/rendered-content])]
      (t/is
       (re-find #"example" rendered-str))

      (t/is (= rendered-str (slurp "./test-resources/fab/outputs/test-file.html")))

      (io/delete-file (io/file "./test-resources/fab/inputs/test-file.html.fab"))
      (io/delete-file (io/file "./test-resources/fab/outputs/test-file.html"))))

  ;; the rerender fn works when tested in isolation, but not
  ;; when used via send!

  (t/testing "ability to manage server state using send and draft!"
    (let [url "http://localhost:9223"]

      (println "0. overriding default state")
      (send test-state (constantly test-config))
      (await test-state)

      (t/is (= "./test-resources/fab/inputs/"
               (get-in @test-state
                       [:site.fabricate/settings
                        :site.fabricate.file/input-dir]))
            "Default configuration should be overridden")

      (println "1. starting application")
      (send-off test-state draft!)
      (await test-state)
      (t/is (#{200 304} (:status (curl/get url {:throw false})))
            "Server should start via agent")

      (println "2. initial write")
      (spit "./test-resources/fab/inputs/test-file.html.fab"
            test-fabricate-str)
      (await test-state)
      (await (:site.fabricate.app/watcher @test-state))
      (t/is (not (agent-error state))
            "File writing should not cause errors in state agent")
      (t/is (not (agent-error (:site.fabricate.app/watcher @test-state)))
            "File writing should not cause errors in watcher agent")

      (Thread/sleep 250)
      (let [response (curl/get url {:throw false})]
        (t/is (re-find #"test\-file\.html" (:body response))
              "File should display in list of files after rendering"))
      (t/is (#{200 304} (:status (curl/get (str url "/test-file.html") {:throw false})))
            "File should be visible on server")
      (Thread/sleep 250)
      (println "3. file update")
      (spit "./test-resources/fab/inputs/test-file.html.fab"
            extra-content-str
            :append true)
      (await test-state)
      (Thread/sleep 250)
      (t/is (re-find #"four" (:body (curl/get (str url "/test-file.html") {:throw false})))
            "File should have contents updated by filewatcher")

      (Thread/sleep 250)
      (println "4. shutdown")
      (send-off test-state stop!)

      (await test-state)

      (t/is (nil? (:status (curl/get url {:throw false})))
            "Server should shutdown via agent")))

  (println "deleting test dir")
  (delete-directory-recursive (io/file "test-resources/fab")))
