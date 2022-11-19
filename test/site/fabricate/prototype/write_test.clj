(ns site.fabricate.prototype.write-test
  (:require [site.fabricate.prototype.write :as write :refer :all]
            [site.fabricate.prototype.read :as read]
            [site.fabricate.prototype.fsm :as fsm]
            [site.fabricate.prototype.test-utils
             :as tu
             :refer [with-instrumentation]]
            [site.fabricate.sketch :as sketch]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.data]
            [malli.core :as m]
            [malli.error :as me]
            [malli.util :as mu]
            [http.server :as server]
            [clojure.test :as t]
            [clojure.pprint :as pprint]
            [babashka.curl :as curl]
            [com.brunobonacci.mulog :as u])
  (:import  [java.util.concurrent Executors]))

(declare test-state)

(u/set-global-context!
 {:clojure.test/namespace (str *ns*)})

(def test-logger
  (u/start-publisher!
   (-> default-site-settings
       (get :site.fabricate.app.logger/config))))

(def test-operations
  (dissoc default-operations rendered-state))

(t/use-fixtures :once with-instrumentation
  (fn reset-log-context [f] (f) (u/set-global-context! {})))

(t/deftest file-utils
  (t/testing "output path fn"
                                        ;  (u/with-context {:clojure.test/contexts t/*testing-contexts*}
                                        ;    (u/trace ::output-fn [:log/level 900]
    (t/is (= "./docs/test-file.txt"
             (get-output-filename "./pages/test-file.txt"
                                  "./pages"
                                  "./docs")))
                                        ;             ))
    ))

(t/deftest page-fsm
  (t/testing "page metadata"
    (let [ex-file (io/file "./content/test-file.txt.fab")]
      (t/is (= {:site.fabricate.page/namespace (symbol 'test-ns)
                :site.fabricate.page/parsed-content
                [{:exec '(ns test-ns)
                  :expr-src "(ns test-ns)"}]
                :site.fabricate.file/filename "content/test-file"
                :site.fabricate.file/output-extension "txt"
                :site.fabricate.file/template-suffix ".fab"
                :site.fabricate.file/output-file "./pages/test-file.txt"
                :site.fabricate.file/input-file ex-file}
               (populate-page-meta
                {:site.fabricate.page/parsed-content [{:exec '(ns test-ns)
                                                       :expr-src "(ns test-ns)"}]
                 :site.fabricate.file/input-file ex-file}
                {:site.fabricate.file/output-dir "./pages"
                 :site.fabricate.file/input-dir "./content"}))))

    (t/is (m/validate (-> populate-page-meta
                          var
                          meta
                          :malli/schema)
                      populate-page-meta))))

(t/deftest doc-rendering

  (doseq [e (:examples (m/properties input-state))]
    (t/testing (str "example file: " e)
      (let [final (fsm/complete test-operations e initial-state)
            final-debug (fsm/complete
                         test-operations
                         {:state/value e} initial-state)]
        (t/is
         (contains?
          final
          :site.fabricate.page/rendered-content)
         "Example FSMs should complete successfully")
        (t/is (contains? (:state/value final-debug)
                         :site.fabricate.page/rendered-content)
              "Example FSMs should complete in debug mode"))))

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
        (t/testing (str ": " page-path)
          (fsm/complete default-operations page-path initial-state))))))

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
  (let [prior-exec clojure.lang.Agent/soloExecutor
        test-exec (Executors/newWorkStealingPool 20)]
    (set-agent-send-executor! test-exec)
    (set-agent-send-off-executor! test-exec)
    (def test-state
      (agent initial-state
             :meta {:context :site.fabricate/app-test
                    :malli/schema state-schema}
             :validator
             #(do
                (t/is (valid-schema?
                       state-schema
                       %
                       "App state should conform after modification"))
                (valid-state? %))
             #_#_:error-handler (fn [a err]
                                  (t/is (valid-state? @a)
                                        "State should be valid")
                                  (pprint (me/humanize
                                           (explain-state @a))))
             :error-mode :continue))
    (println "creating test dir")

    (u/update-global-context!
     merge (tu/gather-test-meta))
    (io/make-parents "./test-resources/fab/outputs/.nothing")
    (io/make-parents "./test-resources/fab/inputs/.nothing")

    (t/testing "rerender fn"
      (u/update-global-context!
       merge (tu/gather-test-meta))
      (let [f (do (spit "./test-resources/fab/inputs/test-file.html.fab"
                        test-fabricate-str)
                  "./test-resources/fab/inputs/test-file.html.fab")
            res (rerender test-config
                          {:file (io/file f) :count 1 :action :create})
            rendered-str (get res :site.fabricate.page/rendered-content)]
        (t/is (valid-schema? page-metadata-schema res)
              "Rerender fn should return valid page maps")
        (t/is
         (re-find #"example" rendered-str))

        (t/is (= rendered-str (slurp "./test-resources/fab/outputs/test-file.html")))

        (io/delete-file (io/file "./test-resources/fab/inputs/test-file.html.fab"))
        (io/delete-file (io/file "./test-resources/fab/outputs/test-file.html"))

        (t/testing "in the context of an agent"
          (let [rerender-agent (agent initial-state)
                agent-valid?
                (-> rerender-agent
                    (send-off rerender {:file (io/file f) :count 1 :action :create})
                    (#(do (await %) %))
                    deref
                    (valid-state?))]
            (t/is agent-valid? "rerender should work with send-off")))))

    ;; the rerender fn works when tested in isolation (with send and with regular calls)
    ;; but not when used via send in the application context

    (t/testing "ability to manage server state using send and draft!"
      (u/update-global-context!
       merge (tu/gather-test-meta))
      (let [url "http://localhost:9223"]

        (add-watch test-state
                   :test-validity
                   (fn [k reff old-state new-state]
                     (t/is (valid-schema? state-schema old-state) "Old state should be valid")
                     (t/is (valid-schema? state-schema new-state) "New state should be valid")))
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
        (t/is (some? (type (get @test-state :site.fabricate.app/server)))
              "Server should be found in state agent")

        (t/is (some? (type (get @test-state :site.fabricate.app/watcher)))
              "File watcher should be found in state agent")

        (t/is (some? (type (get @test-state :site.fabricate.app/logger)))
              "App should launch logger")
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
        (Thread/sleep 500)
        (t/is (re-find #"four" (:body (curl/get (str url "/test-file.html") {:throw false})))
              "File should have contents updated by filewatcher")

        (Thread/sleep 250)
        (println "4. shutdown")
        (send-off test-state stop!)

        (await test-state)

        (t/is (nil? (:status (curl/get url {:throw false})))
              "Server should shutdown via agent")
        (t/is (= :stopped (:site.fabricate.app/server @test-state))
              "Server shutdown should be indicated in state map")
        (t/is (= :stopped (:site.fabricate.app/watcher @test-state))
              "Watcher shutdown should be indicated in state map")))

    (println "deleting test dir")
    (delete-directory-recursive (io/file "test-resources/fab"))

    (println "stopping")
    (set-agent-send-executor! prior-exec)
    (set-agent-send-off-executor! prior-exec)))

(comment
  ;; another method that doesn't work because all
  ;; clojure.test/*testing-contexts* assignments are
  ;; thread-local with (binding ...)


  (add-watch #'t/*testing-vars*
             :var-update
             (fn [k reff old new]
               (println new)
               (u/update-global-context!
                merge (tu/gather-test-meta))))

  (add-watch #'t/*testing-contexts*
             :context-update
             (fn [k reff old new]
               (println new)
               (u/update-global-context!
                merge (tu/gather-test-meta)))))
