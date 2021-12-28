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

(def default-site-settings
  {:site.fabricate.file/template-suffix ".fab"
   :site.fabricate.file/input-dir "./pages"
   :site.fabricate.file/output-dir "./docs"
   :site.fabricate.server/config
   {:cors-allow-headers nil
    :dir (str (System/getProperty "user.dir") "/docs")
    :port 8002
    :no-cache true}})

(def state-schema
  "Schema for the agent containing the state of fabricate."
  [:map
   [:site.fabricate/settings
    [:map
     [:site.fabricate.file/input-dir :string]
     [:site.fabricate.file/output-dir :string]
     [:site.fabricate.file/template-suffix :string]
     [:site.fabricate.server/config :map]]]
   [:site.fabricate/pages [:map-of :string page-metadata-schema]]
   [:site.fabricate.app/watcher {:optional true}
    [:fn #(instance? clojure.lang.Agent %)]]
   [:site.fabricate.app/server {:optional true} :any]])

(def initial-state {:site.fabricate/settings default-site-settings
                    :site.fabricate/pages {}})

(def state
  "This agent holds the current state of all the pages created by fabricate, the application settings, and the state of the application itself"
  (agent initial-state))

(def template-suffix-regex
  (let [suffix (:site.fabricate.file/template-suffix default-site-settings)]
    (re-pattern (str "#*[.]" (subs suffix 1 (count suffix)) "$"))))

(defn template-str->hiccup
  "Attempts to parse the given string"
  {:malli/schema [:=> [:cat :string :map] [:vector :any]]}
  ([content-str {:keys [page-fn path]
                 :or {page-fn (comp read/eval-all read/parse)
                      path "[no file path given]"}}]
   (try
     (page-fn content-str)
     (catch Exception e
       (do (println (format "Caught an exception for %s: \n\n %s"
                            path (.getMessage e)))
           ::read/parse-error))))
  ([content-str] (template-str->hiccup content-str {})))

(defn get-output-filename
  {:malli/schema [:=> [:cat :string :string :string] :string]}
  ([path in-dir out-dir]
   (clojure.string/replace
    path (re-pattern (str "^" in-dir))
    out-dir)))

(defn hiccup->html-str
  {:malli/schema [:=> [:cat :keyword
                       [:fn (::html/head html/element-validators)]
                       [:fn (::html/body html/element-validators)]]
                  :string]}
  [[tag head body]]
  (str "<!DOCTYPE html>\n<html>"
       (hiccup/html {:escape-strings? false} head)
       (hiccup/html body)
       "</html>"))

(defn get-template-files
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
                (or output-file
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

(def input-state
  [:and {:fsm/description "Fabricate input path represented as string"}
   :string [:fn #(.endsWith % ".fab")]])

(def file-state
  [:map {:closed true
         :fsm/description "Fabricate input represented as java.io.File entry in page map"}
   [:site.fabricate.file/input-file [:fn sketch/file?]]
   [:site.fabricate.file/filename :string]])

(def read-state
  [:map {:closed true
         :fsm/description "Fabricate input read in as string"}
   [:site.fabricate.file/input-file [:fn sketch/file?]]
   [:site.fabricate.file/filename :string]
   [:site.fabricate.page/unparsed-content :string]])

(def parsed-state
  (m/schema
   [:map {:closed true
          :fsm/description "Fabricate input parsed and metadata associated with page map"}
    [:site.fabricate.file/input-file [:fn sketch/file?]]
    [:site.fabricate.file/template-suffix
     [:enum (:site.fabricate.file/template-suffix default-site-settings)]]
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

(def evaluated-state
  (mu/closed-schema
   (mu/merge
    parsed-state
    (m/schema
     [:map
      {:closed true
       :fsm/description "Fabricate contents evaluated after parsing"}
      [:site.fabricate.page/evaluated-content [:fn vector?]]
      [:site.fabricate.page/metadata {:optional true} [:map {:closed false}]]]))))

#_(def evaluated-state
  [:map {:closed true
         :fsm/description "Fabricate evaluated under :evaluated-content"}
   [:input-file [:fn sketch/file?]]
   [:unparsed-content :string]
   [:parsed-content [:fn vector?]]
   [:evaluated-content [:fn vector?]]
   [:site.fabricate.page/file-suffix
    [:enum (:site.fabricate.page/file-suffix default-site-settings)]]
   [:filename :string]
   [:file-extension :string]
   [:namespace :symbol]
   [:metadata [:map {:closed false}]]
   [:page-style {:optional true} :string]
   [:title {:optional true} :string]
   [:output-file {:optional true}
    [:orn
     [:path :string]
     [:file [:fn sketch/file?]]]]])

(def html-state
  (mu/closed-schema
   (mu/merge
    evaluated-state
    [:map {:closed true
           :fsm/description "Fabricate input evaluated as hiccup vector"}
     [:site.fabricate.file/output-extension [:enum  "html"]]])))

(def markdown-state
  (mu/closed-schema
   (mu/merge
    evaluated-state
    [:map {:closed true
           :fsm/description "Fabricate markdown input evaluated as markdown string"}
     [:site.fabricate.file/output-extension [:enum  "md" "markdown"]]])))

(defn evaluated->hiccup
  "Takes the evaluated contents and turns them into a well-formed
   hiccup data structure."
  {:malli/schema [:=> [:cat evaluated-state] :map]}
  [{:keys [site.fabricate.page/namespace
           site.fabricate.page/metadata
           site.fabricate.page/evaluated-content]
    :as page-data}]
  (let [body-content (into [:article {:lang "en"}]
                           (page/parse-paragraphs
                            evaluated-content))]
    (list
     (page/doc-header metadata)
     [:article body-content]
     [:footer
      [:div [:a {:href "/"} "Home"]]])))

(def rendered-state
  (mu/merge
   evaluated-state
   [:map {:fsm/description "Fabricate input rendered to output string"
          :open true
          :fsm/state :fsm/exit}         ; indicating the exit state
    [:site.fabricate.page/rendered-content :string]]))

(defn parse-page
  {:malli/schema [:=> [:cat read-state] parsed-state]}
  [{:keys [site.fabricate.page/unparsed-content
           site.fabricate.file/filename] :as page-data}]
  (let [parsed (read/parse unparsed-content {:filename filename})]
    (-> page-data
        (assoc :site.fabricate.page/parsed-content parsed)
        (populate-page-meta default-site-settings))))

(defn eval-parsed-page
  {:malli/schema [:=> [:cat parsed-state] evaluated-state]}
  [{:keys [site.fabricate.page/parsed-content
           site.fabricate.page/namespace] :as page-data}]
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
;; to accept both a page map and a settings map
(def operations
  {input-state (fn [f] {:site.fabricate.file/input-file (io/as-file f)
                        :site.fabricate.file/filename f})
   file-state (fn [{:keys [site.fabricate.file/input-file] :as page-data}]
                (assoc page-data :site.fabricate.page/unparsed-content
                       (slurp input-file)))
   read-state parse-page
   parsed-state eval-parsed-page
   markdown-state
   (fn [{:keys [site.fabricate.page/evaluated-content] :as page-data}]
     (assoc page-data :site.fabricate.page/rendered-content (reduce str evaluated-content)))
   html-state
   (fn [page-data]
     (assoc page-data
            :site.fabricate.page/rendered-content
            (-> page-data
                evaluated->hiccup
                (#(hp/html5 {:lang "en"} %)))))
   rendered-state
   (fn [{:keys [site.fabricate.page/rendered-content
                site.fabricate.file/output-file] :as page-data}]
     (do
       (println "writing page content to" output-file)
       (spit output-file rendered-content)
       page-data))})

(defn update-page-map
  {:malli/schema [:=> [:cat :map :string] :map]}
  [old-map page-name]
  (try (update old-map page-name
               (fn [_] (fsm/complete operations page-name)))
       (catch Exception e
         (println (.getMessage e))
         (println "Parse error detected, skipping")
         old-map)))

(defn rerender
  {:malli/schema
   [:=> [:cat [:map [:file :any] [:count :int] [:action :keyword]]
         state-schema]
    state-schema]}
  [{:keys [site.fabricate/settings site.fabricate/pages]
    :as application-state-map}
   {:keys [file count action]}]
  (if (and (#{:create :modify} action)
           (.endsWith (.toString file)
                      (:site.fabricate.file/template-suffix settings)))
    (let [local-file
          (-> file
              read/->dir-local-path
              (#(do (println "re-rendering") % %)))
          updated-page (fsm/complete operations local-file)]
      (do
        (println "rendered")
        (assoc-in application-state-map
                  [:site.fabricate/pages local-file]
                  updated-page)))))

(comment
  (fsm/complete
   operations
   (first
    (get-template-files
     (:site.fabricate.file/input-dir default-site-settings)
     (:site.fabricate.file/template-suffix default-site-settings))))

  (def example-srv (server/start {:port 8015}))

  (clojure.repl/doc server/start)

  (server/stop example-srv)

  )

(defn draft!
  {:malli/schema [:=> [:cat state-schema] state-schema]}
  [{:keys [site.fabricate/settings site.fabricate/pages]
    :as application-state-map}]
  (let [written-pages
        (into {}
              (for [fp (get-template-files
                        (:site.fabricate.file/input-dir settings)
                        (:site.fabricate.file/template-suffix settings)) ]
                [fp (fsm/complete operations fp)]))
        fw (do
             (println "establishing file watch")
             (watch-dir (fn [f]
                          (send state rerender f))
                        (io/file (:site.fabricate.file/input-dir settings))))
        srv (do
              (println "launching server")
              (server/start (:site.fabricate.server/config settings)))]
    (assoc
     application-state-map
     :site.fabricate/pages written-pages
     :site.fabricate.app/watcher fw
     :site.fabricate.app/server srv)))

(defn publish!
  {:malli/schema
   [:=> [:cat [:map [:files [:* :string]]
               [:dirs [:* :string]]]]
    :nil]}
  [{:keys [files dirs]
    :as opts}]
  (let [all-files
        (apply concat files
               (map #(get-template-files
                      %
                      (:template-suffix default-site-settings)) dirs))]
    (doseq [fp all-files]
      (fsm/complete operations fp))))

(defn stop!
  "Shuts down fabricate's stateful components."
  [application-state-map]
  (-> application-state-map
      (update :site.fabricate.file/watcher
              #(do
                 (println "closing file watcher")
                 (try (do (when (instance? clojure.lang.Agent %)  (close-watcher %)) nil)
                      (catch Exception e nil))))
      (update :site.fabricate/server
              #(do
                 (println "stopping Aleph server")
                 (try (do (server/stop %) nil)
                      (catch Exception e nil))))))

(.addShutdownHook (java.lang.Runtime/getRuntime)
                  (Thread. (fn []
                             (do (println "shutting down")
                                 (send stop! state)
                                 (shutdown-agents)))))

(comment
  (publish {:dirs ["./pages"]})

  (send state draft!)

  (keys @state)

  (type (:site.fabricate.app/server @state))

  (restart-agent state initial-state)

  (send state stop!)

  )

(comment
  ;; to update pages manually, do this:

  (fsm/complete operations "./README.md.fab")

  (fsm/complete operations "./pages/finite-schema-machines.html.fab")

  (fsm/complete operations "./pages/extended-forms.html.fab")

  (site.fabricate.prototype.read.grammar/template
   (slurp "./pages/extended-forms.html.fab"))

  (fsm/complete operations "./pages/fabricate.html.fab")

  (def finite-schema-machines (fsm/complete operations "./pages/finite-schema-machines.html.fab"))

  (malli.error/humanize (m/explain parsed-state finite-schema-machines))

  (def fsm-post-data (get @pages "pages/finite-schema-machines.html.fab"))

  (keys fsm-post-data)

  (:evaluated-content fsm-post-data)
  (hp/html5 (list [:head [:title "something"] [:body "something else"]])))
