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

(def pages
  "This variable holds the current state of all the pages created
   by fabricate."
  (atom {}))

(def page-schema
  ""
  [:map
   [:page-meta sketch/page-metadata-schema]
   [:state (into [:enum] (keys sketch/rerender-state-machine))]])

(def pages-schema
  [:map-of :string page-schema])

(def default-site-settings
  {:template-suffix ".fab"
   :input-dir "./pages"
   :output-dir "./docs"})

(def template-suffix-regex
  (let [suffix (:template-suffix default-site-settings)]
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

(comment (get-output-filename "./pages/test-file.txt.fab" "./pages"
                              "./docs")

         (get-output-filename "./content/test-file.txt.fab"
                              "./content"
                              "./docs"))

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

;; consider moving this to the page namespace
(defn populate-page-meta
  {:malli/schema
   [:=> [:cat sketch/page-metadata-schema [:? :map]]
    sketch/page-metadata-schema]}
  ([{:keys [namespace parsed-content output-file input-file] :as page-data}
    {:keys [input-dir output-dir] :as site-settings}]
   (-> page-data
       (assoc :namespace (or (read/yank-ns parsed-content)
                             namespace
                             (symbol (str "tmp-ns." (Math/abs (hash parsed-content))))))
       (merge (read/get-file-metadata (.getPath input-file)))
       (merge (-> parsed-content
                  read/get-metadata
                  last
                  (select-keys [:namespace :output-file :title])))
       (#(assoc % :output-file
                (or output-file
                    (get-output-filename
                     (str "./" (% :filename)
                          "." (% :file-extension))
                     input-dir
                     output-dir)))))))

(comment
  (def =>populate-page-meta
    [:=> [[sketch/page-metadata-schema [:? :map]]] sketch/page-metadata-schema])

  (meta (var populate-page-meta)))

;; fsm based implementation here
;;
;; write the succession of states, then fill in the schemas
;; describing the desired states, and the functions
;; that produce that particular succession of states

(def input-state
  [:and {:fsm/description "Fabricate input path represented as string"}
   :string [:fn #(.endsWith % ".fab")]])

(comment (m/validate input-state "./README.md.fab"))

(def file-state
  [:map {:closed true
         :fsm/description "Fabricate input represented as :input-file java.io.File entry in page map"}
   [:input-file [:fn sketch/file?]]])

(def read-state
  [:map {:closed true
         :fsm/description "Fabricate input read in as string under :unparsed-content"}
   [:input-file [:fn sketch/file?]]
   [:unparsed-content :string]])

(def parsed-state
  [:map {:closed true
         :fsm/description "Fabricate input parsed under :parsed-content and metadata associated with page map"}
   [:input-file [:fn sketch/file?]]
   [:fabricate/suffix [:enum (:template-suffix default-site-settings)]]
   [:filename :string]
   [:file-extension :string]
   [:unparsed-content :string]
   [:parsed-content [:fn vector?]]
   [:title {:optional true} :string]
   [:namespace {:optional true}
    [:orn [:name :symbol]
     [:form [:fn schema/ns-form?]]]]
   [:page-style {:optional true} :string]
   [:output-file {:optional true}
    [:orn
     [:path :string]
     [:file [:fn sketch/file?]]]]])

(def evaluated-state
  [:map {:closed true
         :fsm/description "Fabricate evaluated under :evaluated-content"}
   [:input-file [:fn sketch/file?]]
   [:unparsed-content :string]
   [:parsed-content [:fn vector?]]
   [:evaluated-content [:fn vector?]]
   [:fabricate/suffix [:enum (:template-suffix default-site-settings)]]
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
     [:file-extension [:enum  "html"]]])))

(def markdown-state
  (mu/closed-schema
   (mu/merge
    evaluated-state
    [:map {:closed true
           :fsm/description "Fabricate markdown input evaluated as markdown string"}
     [:file-extension [:enum  "md" "markdown"]]])))

(defn evaluated->hiccup
  "Takes the evaluated contents and turns them into a well-formed
   hiccup data structure."
  {:malli/schema [:=> [:cat evaluated-state] :map]}
  [{:keys [namespace metadata evaluated-content]
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
    [:rendered-content :string]]))

(defn eval-parsed-page
  [{:keys [parsed-content namespace] :as page-data}]
  (let [nmspc (create-ns namespace)
        evaluated (read/eval-all parsed-content true nmspc)
        page-meta (let [m (ns-resolve nmspc 'metadata)]
                    (if (nil? m) {} (var-get m)))
        metadata
        (page/lift-metadata evaluated page-meta)]
    (assoc page-data :evaluated-content evaluated
           :namespace
           (if (symbol? namespace)
             namespace
             (second (second namespace)))
           :metadata metadata)))

(def operations
  {input-state (fn [f] {:input-file (io/as-file f)})
   file-state (fn [{:keys [input-file] :as page-data}]
                (assoc page-data :unparsed-content (slurp input-file)))
   read-state
   (fn [{:keys [unparsed-content] :as page-data}]
     (let [parsed (read/parse unparsed-content)]
       (-> page-data
           (assoc :parsed-content parsed)
           (populate-page-meta default-site-settings))))
   parsed-state eval-parsed-page
   markdown-state
   (fn [{:keys [evaluated-content] :as page-data}]
     (assoc page-data :rendered-content (apply str evaluated-content)))
   html-state
   (fn [page-data]
     (assoc page-data
            :rendered-content
            (-> page-data
                evaluated->hiccup
                (#(hp/html5 {:lang "en"} %)))))
   rendered-state
   (fn [{:keys [rendered-content output-file] :as page-data}]
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
   [:=> [:cat [:map [:file :any] [:count :int] [:action :keyword]]]
    :nil]}
  [{:keys [file count action]}]
  (if (and (#{:create :modify} action)
           (.endsWith (.toString file)
                      (:template-suffix default-site-settings)))
    (let [local-file (read/->dir-local-path file)]
      (do
        (println "re-rendering" local-file)
        (swap! pages #(update-page-map % local-file))
        (println "rendered")))))

(def default-server-opts
  {:cors-allow-headers nil,
   :dir (str (System/getProperty "user.dir") "/docs"),
   :port 8000,
   :no-cache true})

(defn draft
  {:malli/schema [:=> [:cat] [:fn #(.isInstance clojure.lang.Agent %)]]}
  []
  (do
     ;; (load-deps)
    (doseq [fp (get-template-files
                (:input-dir default-site-settings)
                (:template-suffix
                 default-site-settings))]
      (swap! pages #(update-page-map % fp)))
    (let [srv (do
                (println "launching server")
                (server/start default-server-opts))
          fw (do
               (println "establishing file watch")
               (watch-dir rerender (io/file (:input-dir default-site-settings))))]
      (.addShutdownHook (java.lang.Runtime/getRuntime)
                        (Thread. (fn []
                                   (do (println "shutting down")
                                       (close-watcher fw)
                                       (server/stop srv)
                                       (shutdown-agents)))))
      fw)))

(defn publish
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

(comment
  (publish {:dirs ["./pages"]})

  (def drafts (draft))

  (close-watcher drafts))

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
