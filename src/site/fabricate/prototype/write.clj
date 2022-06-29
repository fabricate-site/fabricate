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
   [http.server :as server]))

(def page-metadata-schema
  "Schema describing the keys and values used by Fabricate to store metadata about pages."
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
    [:orn [:undefined nil?]
     [:path :string]
     [:file [:fn sketch/file?]]]]])


(def state-schema
  "Schema for the agent containing the state of fabricate."
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
    [:fn #(instance? clojure.lang.Agent %)]]
   [:site.fabricate.app/server {:optional true} :any]])

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
             (read/parse unparsed-content {:filename filename}))
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
        evaluated (read/eval-all parsed-content true nmspc)
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
  "Default render loop for Fabricate projects."
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
                           (reduce str evaluated-content)))
   html-state (fn render-hiccup
                [page-data state]
                (let [final-hiccup (evaluated->hiccup page-data state)]
                  (assoc page-data
                         :site.fabricate.page/evaluated-content final-hiccup
                         :site.fabricate.page/rendered-content
                         (str (hiccup/html
                               {:escape-strings? false}
                               final-hiccup)))))
   rendered-state
   (fn write-file
     [{:keys [site.fabricate.page/rendered-content
              site.fabricate.file/output-file] :as page-data}
      settings]
     (do
       (println "writing page content to" output-file)
       (spit output-file rendered-content)
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
    :no-cache true}})

(def initial-state
  "Starting state for Fabricate's application components."
  {:site.fabricate/settings default-site-settings
   :site.fabricate/pages {}})

(def state
  "This agent holds the current state of all the pages created by Fabricate, the application settings, and the state of the application itself"
  (agent initial-state))


(defn rerender
  "Render the given page on file change."
  {:malli/schema
   [:=> [:cat [:map [:file :any] [:count :int] [:action :keyword]]
         state-schema]
    state-schema]}
  [{:keys [site.fabricate/settings site.fabricate/pages]
    :as application-state-map}
   {:keys [file count action]}]
  (let [{:keys [site.fabricate.file/template-suffix
                site.fabricate.file/output-dir
                site.fabricate.file/operations]}
        settings]
    (if (and (#{:create :modify} action)
             (.endsWith (.toString file) template-suffix))
      (let [local-file
            (-> file
                read/->dir-local-path
                (#(do (println "re-rendering") % %)))
            updated-page
            (fsm/complete operations
                          local-file
                          application-state-map)]
        (do
          (println "rendered")
          (assoc-in application-state-map
                    [:site.fabricate/pages local-file]
                    updated-page))))))

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
  "Render all the Fabricate templates once, then launches a file watcher to rerender the templates on save. Also launches a web server to view the rendered pages locally."
  {:malli/schema [:=> [:cat state-schema] state-schema]}
  [{:keys [site.fabricate/settings site.fabricate/pages]
    :as application-state-map}]
  (let [{:keys [site.fabricate.file/input-dir
                site.fabricate.file/template-suffix
                site.fabricate.file/operations]} settings
        srv (future (do
                      (println "launching server")
                      (server/start (:site.fabricate.server/config settings))))
        written-pages
        (future
          (->> (get-template-files input-dir template-suffix)
               (pmap (fn [fp] [fp (fsm/complete operations fp application-state-map)]))
               (into {})))
        fw
        (future
          (do
            (println "establishing file watch")
            (let [state-agent *agent*
                  fw (watch-dir
                      (fn [f]
                        (do (send-off state-agent rerender f)))
                      (io/file input-dir))]
              #_(set-error-mode! fw :continue)
              fw)))]
    (assoc
     application-state-map
     :site.fabricate/pages @written-pages
     :site.fabricate.app/watcher @fw
     :site.fabricate.app/server @srv)))

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
              #(do
                 (println "closing file watcher")
                 (try (do (when (instance? clojure.lang.Agent %)  (close-watcher %)) nil)
                      (catch Exception e nil))))
      (update :site.fabricate.app/server
              #(do
                 (println "stopping file server")
                 (when % (do (server/stop %) nil))
                 #_(try (do (server/stop %) nil)
                        (catch Exception e nil))))))

(.addShutdownHook (java.lang.Runtime/getRuntime)
                  (Thread. (fn []
                             (do (println "shutting down")
                                 #_(send state stop!)
                                 (shutdown-agents)))))

(comment
  (publish {:dirs ["./pages"]})


  (-> state
      (send (constantly initial-state))
      (send-off draft!))

  (-> state
      (send-off stop!))


  (keys @state)

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
                "./pages/background/finite-schema-machines.html.fab"
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
         (println "watching output dir for changes")
         (let [output-dir (:site.fabricate.file/output-dir settings)
               out-dir-trailing (if (not (.endsWith output-dir "/"))
                                  (str output-dir "/") output-dir)]
           (assoc application-state-map
                  :site.fabricate.file.output/watcher
                  (watch-dir
                   (fn [{:keys [file count action]}]
                     (if (#{:create :modify} action)
                       (do
                         (println "syncing")
                         (let [r (clojure.java.shell/sh "sync-fabricate.sh")]
                           (println (or (:out r) (:err r)))))))
                   (io/file output-dir)))))) )


  )
