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
   [hiccup.page :as hp]
   [malli.core :as m]
   [malli.util :as mu]
   [malli.generator :as mg]
   [site.fabricate.sketch :as sketch]
   [site.fabricate.prototype.read :as read]
   [site.fabricate.prototype.html :as html]
   [site.fabricate.prototype.page :as page]
   [site.fabricate.prototype.schema :as schema]
   [juxt.dirwatch :refer [watch-dir close-watcher]]
   ))

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
   :output-dir "./pages"})

(def template-suffix-regex (re-pattern (str "#*[.]" (:template-suffix default-site-settings) "$")))

(defn template-str->hiccup
  "Attempts to parse the given string"
  ([content-str {:keys [page-fn path]
                 :or {page-fn (comp read/eval-with-errors read/parse)
                      path "[no file path given]"}}]
   (try
     (page-fn content-str)
     (catch Exception e
       (do (println (format "Caught an exception for %s: \n\n %s"
                            path (.getMessage e)))
           ::read/parse-error))))
  ([content-str] (template-str->hiccup content-str {})))

(defn get-output-filename
  ([path out-dir]
   (-> path
       io/file
       (.getName)
       (.toString)
       (string/split template-suffix-regex)
       first
       (#(str out-dir "/" %))))
  ([path] (get-output-filename path "./pages")))

(defn hiccup->html-str [[tag head body]]
  (str "<!DOCTYPE html>\n<html>"
       (hiccup/html {:escape-strings? false} head)
       (hiccup/html body)
       "</html>"))






(defn template->hiccup
  "Converts a template file to a hiccup data structure for the page."
  [t]
  (let [parsed (read/parse t)
        form-ns (read/yank-ns parsed)
        evaluated  (read/eval-with-errors
                    parsed form-ns html/validate-element)
        page-meta (ns-resolve form-ns 'metadata)
        body-content
        (into [:article {:lang "en"}]
              page/sectionize-contents
              evaluated)]
    [:html
     (page/doc-header page-meta)
     [:body
      body-content
      [:footer
       [:div [:a {:href "/"} "Home"]]]]]))

(defn get-template-files [dir suffix]
  (->> dir
       io/file
       file-seq
       (filter #(and (.isFile %)
                     (not (.isDirectory %))
                     (.endsWith (.toString %) suffix)))
       (map #(.toString %))))

(defn render-template-file
  ([path page-fn out-dir]
   (let [out-file (get-output-filename path out-dir)]
     (println "Rendering" (.toString out-file))
     (-> path
         slurp
         page-fn
         hiccup->html-str
         (#(spit out-file %))
         )))
  ([path page-fn] (render-template-file path page-fn "pages"))
  ([path]
   (render-template-file path template->hiccup "pages")))

(defn render-template-files
  "Writes the given files. Renders all in the content dir when called without args."
  ([template-files page-fn out-dir]
   (doseq [f template-files]
     (render-template-file f page-fn out-dir)))
  ([template-files page-fn] (render-template-files template-files page-fn "pages"))
  ([template-files] (render-template-files template-files template->hiccup "pages"))
  ([] (render-template-files (get-template-files "content" (:template-suffix default-site-settings)))))



(defn update-page-map [old-map page-name page-contents]
  (if (= ::read/parse-error page-contents)
    (do
      (println "Parse error detected, skipping")
      old-map)
    (assoc old-map page-name
           {:data page-contents
            :html (hiccup->html-str page-contents)})))



(defn write-file! [output-path html-content]
  (if (not
       (nil? html-content))
    (do (println "writing file to" output-path)
        (spit output-path html-content))))

(defn update-and-write! [fp]
  (do (let [f-contents
                 (-> fp slurp
                     (template-str->hiccup {:page-fn template->hiccup
                                            :path fp}))]
             (swap! pages #(update-page-map % fp f-contents)))
           (let [output-path (get-output-filename fp "./pages")
                 html-content (get-in @pages [fp :html])]
             (write-file! output-path html-content))))

(defn rerender [{:keys [file count action]}]
  (if (#{:create :modify} action)
    (do
      (println "re-rendering" (.toString file))
      (update-and-write! (.toString file))
      (println "rendered"))))



(defn draft
  ([]
   (do
     ;; (load-deps)
     (doseq [fp (get-template-files "content" (:template-suffix
                                               default-site-settings))]
       (update-and-write! fp))
     (println "establishing file watch")
     (let [fw (watch-dir rerender (io/file "./content/"))]
       (.addShutdownHook (java.lang.Runtime/getRuntime)
                         (Thread. (fn []
                                    (do (println "shutting down")
                                        (close-watcher fw)
                                        (shutdown-agents)))))
       (loop [watch fw]
         (await fw)
         (recur [fw]))))))

(comment
  (future (draft)))

(defn publish
  ([{:keys [files dirs]
     :as opts}]
   (let [all-files
         (apply concat files
                (map #(get-template-files
                       %
                       (:template-suffix default-site-settings)) dirs))]
     (render-template-files all-files))))

;; fsm based implementation here
;;
;; write the succession of states, then fill in the schemas
;; describing the desired states, and the functions
;; that produce that particular succession of states

(def operations
  {[:and :string [:fn #(.exists (io/file %))]]
   {:op (fn [f] {:input-file (io/as-file f)})
    :description "Representing input path as :input-file entry in page data map"
    :target-state :page-map}
   (-> sketch/page-metadata-schema
       (mu/dissoc :output-file)
       (mu/dissoc :title)
       (mu/dissoc :namespace)
       (mu/dissoc :page-style)
       (mu/dissoc :unparsed-content)
       (mu/dissoc :parsed-content)
       (mu/dissoc :hiccup-content)
       (mu/dissoc :rendered-content))
   {:op (fn [{:keys [input-file] :as page-data}]
          (assoc page-data :unparsed-content (slurp input-file)))
    :description "Reading in the page content as a string"
    :target-state :input-read}
   #_(-> sketch/page-metadata-schema
       (mu/dissoc :output-file)
       (mu/dissoc :title)
       (mu/dissoc :namespace)
       (mu/dissoc :page-style)
       (mu/dissoc :parsed-content)
       (mu/dissoc :hiccup-content)
       (mu/dissoc :rendered-content))
   })

(defn write-page!
  "Writes the page. Returns a tuple with the resulting state and the page contents."
  [{:keys [output-file page-content]
    :as page-data}]
  (let [state
        (if
            (not (string? page-content)) ::write-error
            (try (do (spit page-content) ::written)
                 (catch Exception e ::write-error)))]
    [state page-data]))

(def =>write-page! [:=> [:cat sketch/published-page-metadata-schema]
                    [:catn [:state [:enum ::write-error ::written]]
                     [:page-data sketch/published-page-metadata-schema]]])

(comment
  (m/validate =>write-page! write-page!
              {::m/function-checker mg/function-checker}))

;; consider moving this to the page namespace
(defn populate-page-meta
  {:malli/schema
   [:=> [:cat sketch/page-metadata-schema [:? :map]]
    sketch/page-metadata-schema]}
  ([{:keys [namespace page-content output-file input-file] :as page-data}
    {:keys [output-dir] :as site-settings}]
   (assoc page-data
          :namespace (or (read/yank-ns page-content)
                         namespace
                         (symbol (str "tmp-ns." (Math/abs (hash page-content)))))
          :output-file (or output-file
                           (get-output-filename input-file
                                                output-dir)))))

(comment
  (def =>populate-page-meta
    [:=> [[sketch/page-metadata-schema [:? :map]]] sketch/page-metadata-schema])

  (meta (var populate-page-meta))

  )
