(ns site.fabricate.prototype.write
  "Fabricate's namespace for writing HTML pages. This file combines
   fabricate's other namespaces for reading source files, parsing
   & restructuring their contents, and generating HTML pages with
   additional functions for input and output in order to achieve the
   purpose of the library: create HTML documents.

  The central method that it uses to combine these functions from
  other namespaces is a finite-state-machine."
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.set :as set :refer [rename-keys]]
   [clojure.pprint :refer [pprint]]
   [hiccup2.core :as hiccup]
   [hiccup.core :as h]
   [hiccup.page :as hp]
   [malli.core :as m]
   [malli.util :as mu]
   [malli.instrument :as mi]
   [malli.generator :as mg]
   [site.fabricate.sketch :as sketch]
   [site.fabricate.prototype.read :as read]
   [site.fabricate.prototype.html :as html]
   [site.fabricate.prototype.page :as page]
   [site.fabricate.prototype.fsm :as fsm]
   [site.fabricate.prototype.schema :as schema]
   [juxt.dirwatch :refer [watch-dir close-watcher]]
   [http.server :as server]
   [com.brunobonacci.mulog :as u]))

(def page-metadata-schema
  "Schema describing the keys and values used by Fabricate to store metadata about pages."
  (m/schema
   [:map
    [:site.fabricate.file/input-file [:orn [:path :string]
                                      [:file [:fn sketch/file?]]]]
    [:site.fabricate.file/template-suffix {:optional true} :string]
    [:site.fabricate.page/parsed-content {:optional true} [:fn vector?]]
    [:site.fabricate.page/title {:optional true} :string]
    [:site.fabricate.page/hiccup-content {:optional true} html/html]
    [:site.fabricate.page/rendered-content {:optional true} :string]
    [:site.fabricate.page/namespace {:optional true} :symbol]
    [:site.fabricate.page/stylesheet {:optional true} :string]
    [:site.fabricate.file/output-file {:optional true}
     [:orn [:undefined :nil]
      [:path :string]
      [:file [:fn sketch/file?]]]]]))

(def state-schema
  "Schema for the agent containing the state of fabricate."
  (m/schema
   [:map
    [:site.fabricate/settings
     [:map
      [:site.fabricate.file/input-dir :string]
      [:site.fabricate.file/output-dir :string]
      [:site.fabricate.file/template-suffix :string]
      [:site.fabricate.server/config :map]
      [:site.fabricate.file/operations fsm/state-action-map]
      [:site.fabricate.page/doc-header ifn?]]]
    [:site.fabricate/pages [:map-of :string page-metadata-schema]]
    [:site.fabricate.app/watcher {:optional true}
     [:orn
      [:running [:fn #(instance? clojure.lang.Agent %)]]
      [:stopped [:= :stopped]]
      [:error/shutdown [:= :error/shutdown]]]]
    [:site.fabricate.app/server {:optional true}
     [:orn
      [:stopped [:= :stopped]]
      [:error/shutdown [:= :error/shutdown]]
      [:running any?]]]
    [:site.fabricate.app/logger {:optional true}
     [:orn [:stopped :nil]
      [:running any?]]]]))

(defn get-output-filename
  "Creates the output filename for the given input"
  {:malli/schema [:=> [:cat :string :string :string] :string]}
  ([path in-dir out-dir]
   (clojure.string/replace
    path (re-pattern (str "^" in-dir))
    out-dir)))

(defn get-template-files
  "Get the Fabricate input template files in the given directory"
  {:malli/schema [:=> [:cat :string :string]
                  [:* :string]]}
  [dir suffix]
  (->> dir
       io/file
       file-seq
       (filter #(and (.isFile %)
                     (not (.isDirectory %))
                     (.endsWith (.toString %) suffix)))
       (map #(.toString %))))

(defn populate-page-meta
  "Get the metadata for the given page."
  {:malli/schema
   [:=> [:cat page-metadata-schema :map]
    page-metadata-schema]}
  [{:keys [site.fabricate.page/namespace
           site.fabricate.page/parsed-content
           site.fabricate.file/output-file
           site.fabricate.file/filename
           site.fabricate.file/input-file]
    :as page-data}
   {:keys [site.fabricate.file/input-dir
           site.fabricate.file/output-dir] :as site-settings}]
  (-> page-data
      (assoc :site.fabricate.page/namespace
             (or (read/yank-ns parsed-content)
                 namespace
                 (symbol (str "tmp-ns." (Math/abs (hash parsed-content))))))
      (merge (read/get-file-metadata (.getPath input-file)))
      (merge (-> parsed-content
                 read/get-metadata
                 last
                 (select-keys [:namespace :output-file :title
                               :site.fabricate.page/namespace
                               :site.fabricate.file/output-file
                               :site.fabricate.page/title])
                 (rename-keys {:namespace :site.fabricate.page/namespace
                               :output-file :site.fabricate.file/output-file
                               :title :site.fabricate.page/title})))
      (#(assoc % :site.fabricate.file/output-file
               (or (:site.fabricate.file/output-file %)
                   output-file
                   (get-output-filename
                    (str "./" (% :site.fabricate.file/filename)
                         "." (% :site.fabricate.file/output-extension))
                    input-dir
                    output-dir))))))


;; fsm based implementation here
;;
;; write the succession of states, then fill in the schemas
;; describing the desired states, and the functions
;; that produce that particular succession of states

;; maybe make these state schema var names less ambiguous
(def input-state
  "Starting state for Fabricate templates: path as a string."
  (m/schema
   [:and {:fsm/description "Fabricate input path represented as string"
          :fsm/name "Input"
          :examples ["pages/background/finite-schema-machines.html.fab"
                     "pages/index.html.fab"
                     "README.md.fab"]}
    :string [:fn #(.endsWith % ".fab")]]))

(def file-state
  "Fabricate input represented as java.io.File entry in page map"
  (m/schema
   [:map {:closed true
          :fsm/name "File"
          :fsm/description "Fabricate input represented as java.io.File entry in page map"}
    [:site.fabricate.file/input-file [:fn sketch/file?]]
    [:site.fabricate.file/filename :string]]))

(def read-state
  "Fabricate input read in as string"
  (m/schema
   [:map {:closed true
          :fsm/name "Read"
          :fsm/description "Fabricate input read in as string"}
    [:site.fabricate.file/input-file [:fn sketch/file?]]
    [:site.fabricate.file/filename :string]
    [:site.fabricate.page/unparsed-content :string]]))


(def parsed-state
  "Fabricate input parsed and metadata associated with page map"
  (m/schema
   [:map {:closed true
          :fsm/name "Parsed"
          :fsm/description "Fabricate input parsed and metadata associated with page map"}
    [:site.fabricate.file/input-file [:fn sketch/file?]]
    [:site.fabricate.file/template-suffix
     [:orn [:default [:= ".fab"]]
      [:custom :string]]]
    [:site.fabricate.file/filename :string]
    [:site.fabricate.file/output-extension :string]
    [:site.fabricate.page/unparsed-content :string]
    [:site.fabricate.page/parsed-content [:fn vector?]]
    [:site.fabricate.page/title {:optional true} :string]
    [:site.fabricate.page/namespace {:optional true} :symbol]
    [:site.fabricate.file/output-file {:optional true}
     [:orn
      [:path :string]
      [:file [:fn sketch/file?]]]]]))

(defn parse-contents
  "Parses the contents of the given template map."
  {:malli/schema [:=> [:cat read-state :map] parsed-state]}
  [{:keys [site.fabricate.page/unparsed-content
           site.fabricate.file/filename]
    :as page-data}
   {:keys [site.fabricate/settings]}]
  (-> page-data
      (assoc :site.fabricate.page/parsed-content
             (u/trace ::parse-template
               {:pairs []}
               (read/parse unparsed-content {:filename filename})))
      (populate-page-meta settings)))

(def evaluated-state
  "Fabricate contents evaluated after parsing"
  (mu/closed-schema
   (mu/merge
    parsed-state
    (m/schema
     [:map
      {:closed true
       :fsm/name "Evaluated"
       :fsm/description "Fabricate contents evaluated after parsing"}
      [:site.fabricate.page/evaluated-content [:fn #(or (list? %) (vector? %))]]
      [:site.fabricate.page/metadata {:optional true} [:map {:closed false}]]]))))

(def html-state
  "Fabricate input evaluated as hiccup vector"
  (mu/closed-schema
   (mu/merge
    evaluated-state
    [:map {:closed true
           :fsm/name "HTML"
           :fsm/description "Fabricate input evaluated as hiccup vector"}
     [:site.fabricate.file/output-extension [:enum  "html"]]])))

(def markdown-state
  "Fabricate markdown input evaluated as markdown string"
  (mu/closed-schema
   (mu/merge
    evaluated-state
    [:map {:closed true
           :fsm/name "Markdown"
           :fsm/description "Fabricate markdown input evaluated as markdown string"}
     [:site.fabricate.file/output-extension [:enum  "md" "markdown"]]])))

(defn evaluated->hiccup
  "Takes the evaluated contents and turns them into a well-formed
   hiccup data structure."
  {:malli/schema [:=> [:cat evaluated-state state-schema]
                  html/html]}
  [{:keys [site.fabricate.page/namespace
           site.fabricate.page/metadata
           site.fabricate.page/evaluated-content]
    :as page-data}
   {:keys [site.fabricate/settings]
    :as state}]
  (let [{:keys [site.fabricate.page/doc-header]} settings
        body-content (into [:article {:lang "en"}]
                           (page/parse-paragraphs
                            evaluated-content))]
    [:html
     (doc-header metadata)
     [:body
      [:main body-content]
      [:footer
       [:div [:a {:href "/"} "Home"]]]]]))

(def rendered-state
  "Fabricate input rendered to output string"
  (mu/merge
   evaluated-state
   [:map {:fsm/description "Fabricate input rendered to output string"
          :fsm/name "Rendered"
          :fsm/side-effect? true ; indicating that the associated state performs a side effect
          :open true
          :fsm/state :fsm/exit}         ; indicating the exit state
    [:site.fabricate.page/rendered-content :string]]))

(defn eval-parsed-page
  "Evaluate the given page after parsing."
  {:malli/schema [:=> [:cat parsed-state :map] evaluated-state]}
  [{:keys [site.fabricate.page/parsed-content
           site.fabricate.page/namespace] :as page-data}
   site-opts]
  (let [nmspc (create-ns namespace)
        evaluated (u/trace ::eval-template
                    {:pairs [:site.fabricate.page/namespace (str namespace)]}
                    (read/eval-all parsed-content true nmspc))
        page-meta (let [m (ns-resolve nmspc 'metadata)]
                    (if (nil? m) {} (var-get m)))
        metadata
        (page/lift-metadata evaluated page-meta)]
    (assoc page-data :site.fabricate.page/evaluated-content
           evaluated
           :site.fabricate.page/namespace
           (if (symbol? namespace)
             namespace
             (second (second namespace)))
           :site.fabricate.page/metadata metadata)))

;; to make the state runtime reconfigurable, rewrite all these functions
;; to accept both a page map and a settings map - or just the entire state
;;
;; and also include these operations in the state map
(def default-operations
  "Default render loop for Fabricate projects. Defined as a mapping of malli schemas describing states to the functions that process data in that state. See the fsm namespace for additional information."
  {input-state (fn input-file
                 [f _] {:site.fabricate.file/input-file (io/as-file f)
                        :site.fabricate.file/filename f})
   file-state (fn read-file
                [{:keys [site.fabricate.file/input-file] :as page-data} _]
                (assoc page-data :site.fabricate.page/unparsed-content
                       (slurp input-file)))
   read-state parse-contents
   parsed-state eval-parsed-page
   markdown-state (fn markdown-str
                    [{:keys [site.fabricate.page/evaluated-content]
                      :as page-data} _]
                    (assoc page-data :site.fabricate.page/rendered-content
                           (u/trace ::evaluated->markdown
                             {:pairs []}
                             (reduce str evaluated-content))))
   html-state (fn render-hiccup
                [{:keys [site.fabricate.file/input-file]
                  :as page-data} state]
                (let [final-hiccup
                      (u/trace ::evaulated->hiccup {:pairs []}
                               (evaluated->hiccup page-data state))]
                  (assoc page-data
                         :site.fabricate.page/evaluated-content final-hiccup
                         :site.fabricate.page/rendered-content
                         (str (u/trace
                                  ::hiccup->html {:pairs []}
                                  (hiccup/html
                                   {:escape-strings? false}
                                   final-hiccup))))))
   rendered-state
   (fn write-file
     [{:keys [site.fabricate.page/rendered-content
              site.fabricate.file/output-file] :as page-data}
      settings]
     (do
       (u/trace ::write-output-file {:pairs []}
                (spit output-file rendered-content))
       page-data))})

(def default-site-settings
  "Default configuration for Fabricate projects."
  {:site.fabricate.file/template-suffix ".fab"
   :site.fabricate.file/input-dir "./pages"
   :site.fabricate.file/output-dir "./docs"
   :site.fabricate.file/operations default-operations
   :site.fabricate.page/doc-header page/doc-header
   :site.fabricate.server/config
   {:cors-allow-headers nil
    :dir (str (System/getProperty "user.dir") "/docs")
    :port 8002
    :no-cache true}
   :site.fabricate.app.logger/config
   {:type :console :pretty? true
    :transform
    (fn [events]
      (->> events
           (filter #(or (<= 800 (:log/level % 0))
                        (= (str *ns*) (:mulog/namespace %))
                        (= (str *ns*) (namespace (:mulog/event-name %)))))
           (map #(dissoc % :state/value :fsm/value))
           distinct))}})

(def initial-state
  "Starting state for Fabricate's application components."
  {:site.fabricate/settings default-site-settings
   :site.fabricate/pages {}})

(comment

  (get-in initial-state
          [:site.fabricate/settings
           :site.fabricate.app.logger/config])

  )

(defn- report-error [a err]
  (u/log ::agent-error :agent/context (:context (meta a))
         :agent/error (Throwable->map err) :agent/type (type @a)
         :log/level 900)
  (let [agent-schema (:malli/schema (meta a))]

    #_(when agent-schema
        (u/log ::agent-schema :malli/schema agent-schema
               :malli/error (m/explain agent-schema @a)))))

(def ^{:malli/schema [:=> [:cat :any] :boolean]}
  valid-state? (m/validator state-schema))
(def ^{:malli/schema [:=> [:cat :any] :map]}
  explain-state (m/explainer state-schema))

(assert (valid-state? initial-state))

(def state
  "This agent holds the current state of all the pages created by Fabricate, the application settings, and the state of the application itself"
  (agent initial-state :meta {:context :site.fabricate/app
                              :malli/schema state-schema}
         :error-handler report-error
         :validator
         (fn [v]
           (let [m? (map? v)]
             (if (not m?)
               (do
                 (throw (java.lang.RuntimeException. "App state not map")))
               true)))
         #_(fn [s] (let [v? (valid-state? s)]
                     (when-not v? (pprint (malli.error/humanize (explain-state s))))
                     v?))
         #_ #_:error-mode :continue))

(comment
  (add-watch
   state
   :monitor-state
   (fn [k reff old-state new-state]
     (u/log ::state-agent
            :agent/context (:context (meta reff))
            :agent/previous-state-valid? (valid-state? old-state)
            :agent/current-state-valid? (valid-state? new-state))))

                                        ; doesn't seem to work.
  (remove-watch state :monitor-state)

  )

(defn rerender
  "Render the given page on file change. Returns the completed page map."
  {:malli/schema
   [:=> [:cat state-schema
         [:map [:file :any] [:count :int] [:action :keyword]]] :map]}
  [{:keys [site.fabricate/settings site.fabricate/pages]
    :as application-state-map}
   {:keys [file count action]}]
  (let [{:keys [site.fabricate.file/template-suffix
                site.fabricate.file/output-dir
                site.fabricate.file/operations]}
        settings]
    (let [local-file (read/->dir-local-path file)]
      (u/with-context
          {:log/level 800
           :site.fabricate.file/filename local-file}
        (u/trace ::rerender
          {:pairs []}
          (fsm/complete operations
                        local-file
                        application-state-map))))))


(comment
  (fsm/complete
   default-operations
   (first
    (get-template-files
     (:site.fabricate.file/input-dir default-site-settings)
     (:site.fabricate.file/template-suffix default-site-settings))))

  (def example-srv (server/start {:port 8015}))

  (clojure.repl/doc server/start)

  (server/stop example-srv)

  )

(defn draft!
  "Render all the Fabricate templates once, then launches a file watcher to rerender the templates on save. Also launches a web server to view the rendered pages locally, and creates a logger that prints page rerender operations in the REPL."
  {:malli/schema [:=> [:cat state-schema] state-schema]}
  [{:keys [site.fabricate/settings site.fabricate/pages]
    :as application-state-map}]
  (let [{:keys [site.fabricate.file/input-dir
                site.fabricate.file/template-suffix
                site.fabricate.file/operations]} settings
        console-logger
        (u/start-publisher!
         (get settings :site.fabricate.app.logger/config))
        srv (future
              (do
                (u/trace ::server-start
                         [:log/level 800]
                         (server/start (:site.fabricate.server/config settings)))))
        written-pages
        (future
          (u/trace ::initial-page-render
                   [:log/level 800]
                   (->> (get-template-files input-dir template-suffix)
                        (pmap (fn [fp] [fp (fsm/complete operations fp application-state-map)]))
                        (into {}))))
        fw
        (future
          (u/trace
           ::file-watcher-start
           [:log/level 800]
           (let [state-agent *agent*
                 fw (watch-dir
                     (fn [{:keys [file action] :as f}]
                       (if (and (#{:create :modify} action)
                                (not (re-find #"#" (.toString file)))
                                (.endsWith (.toString file) template-suffix))
                         (do (send-off
                              state-agent
                              (fn [s]
                                (let [p (rerender s f)
                                      fname (:site.fabricate.file/filename p)]
                                  (assoc-in s [:site.fabricate/pages fname] p))))
                             nil)))
                     (io/file input-dir))]
             (alter-meta! fw assoc :context :site.fabricate.app/watcher)
             (set-error-mode! fw :continue)
             (set-error-handler! fw report-error)
             fw)))]


    (assoc
     application-state-map
     :site.fabricate/pages @written-pages
     :site.fabricate.app/watcher @fw
     :site.fabricate.app/server @srv
     :site.fabricate.app/logger console-logger)))

(defn publish!
  "Render all the fabricate templates."
  {:malli/schema
   [:=> [:cat [:map [:files [:* :string]]
               [:dirs [:* :string]]]]
    :nil]}
  [{:keys [files dirs]
    :as opts}]
  (let [current-state (deref state)
        all-files
        (apply concat files
               (map #(get-template-files
                      %
                      (get-in current-state
                              [:site.fabricate/settings
                               :site.fabricate.file/template-suffix]))
                    dirs))]
    (doseq [fp all-files]
      (fsm/complete
       (get-in current-state
               [:site.fabricate/settings
                :site.fabricate.file/operations])
       fp
       current-state))))

(defn stop!
  "Shuts down fabricate's stateful components."
  {:malli/schema [:=> [:cat state-schema] state-schema]}
  [application-state-map]
  (-> application-state-map
      (update :site.fabricate.app/watcher
              (fn [w]
                (u/trace ::file-watcher-stop
                  [:log/level 800]
                  (try (do (when (instance? clojure.lang.Agent w)
                             (close-watcher w)) :stopped)
                       (catch Exception e :error/shutdown)))))
      (update :site.fabricate.app/server
              (fn [s]
                (u/trace ::server-stop
                  [:log/level 800]
                  (if (and s (not (#{:stopped :error/shutdown} s)))
                    (do (server/stop s) :stopped)
                    s)))
              #_(try (do (server/stop %) nil)
                     (catch Exception e nil)))
      (update :site.fabricate.app/logger
              (fn [l]
                (u/log ::logger-stop
                       [:log/level 800])
                (l)
                nil))))

(.addShutdownHook
 (java.lang.Runtime/getRuntime)
 (Thread. (fn []
            (u/trace ::app-shutdown
              [:log/level 800]
              (shutdown-agents)))))

(comment
  (publish {:dirs ["./pages"]})


  (-> state
      (send (constantly initial-state))
      (send-off draft!))

  (do
    (send-off state stop!)
    nil)

  (keys @state)

  (keys (get-in @state [:site.fabricate/pages]))

  (type (:site.fabricate.app/server @state))

  (restart-agent state initial-state)


  @state

  )

(comment
  ;; to update pages manually, do this:

  (fsm/complete
   default-operations
   "./README.md.fab"
   initial-state )


  (fsm/complete default-operations
                "./pages/reference/namespaces/site.fabricate.prototype.write.html.fab"
                @state)

  (fsm/complete default-operations "./pages/extended-forms.html.fab"
                state)

  (fsm/complete default-operations
                "./pages/reference/template-structure.html.fab"
                @state)

  (fsm/complete default-operations
                "./pages/index.html.fab"
                @state)

  (site.fabricate.prototype.read.grammar/template
   (slurp "./pages/extended-forms.html.fab"))


  (do (fsm/complete default-operations "./pages/reference/fabricate-fsm.html.fab"
                    @state)
      nil)

  (def finite-schema-machines (fsm/complete default-operations "./pages/finite-schema-machines.html.fab"))

  (malli.error/humanize (m/explain parsed-state finite-schema-machines))

  (def fsm-post-data (get @pages "pages/finite-schema-machines.html.fab"))

  (keys fsm-post-data)

  (keys  @state)

  (:site.fabricate.page/doc-header
   (:site.fabricate/settings initial-state))

  (:evaluated-content fsm-post-data)
  (hp/html5 (list [:head [:title "something"] [:body "something else"]])))



(comment

  ;; experiment with adding an output file watcher
  (-> state
      (send (constantly initial-state))
      (send-off draft!)
      (send-off
       (fn [{:keys [site.fabricate/settings]
             :as application-state-map}]
         (let [output-dir (:site.fabricate.file/output-dir settings)
               out-dir-trailing (if (not (.endsWith output-dir "/"))
                                  (str output-dir "/") output-dir)]
           (u/trace ::watch-output-dir
             [:log/level 800
              :site.fabricate.file/output-dir out-dir-trailing]
             (assoc application-state-map
                    :site.fabricate.file.output/watcher
                    (watch-dir
                     (fn [{:keys [file count action]}]
                       (if (#{:create :modify} action)
                         (u/trace ::sync-netlify
                           [:log/level 800
                            :site.fabricate.file/output-dir
                            out-dir-trailing]
                           (do
                             (let [r (clojure.java.shell/sh "sync-fabricate.sh")]
                               #_(println (or (:out r) (:err r))))))))
                     (io/file output-dir))))))) )


  )

(comment
  (namespace ::something)

  )
