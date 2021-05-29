(ns site.fabricate.prototype.write
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [hiccup2.core :as hiccup]
   [hiccup.page :as hp]
   [site.fabricate.prototype.read :as read]
   [site.fabricate.prototype.html :as html]
   [site.fabricate.prototype.page :as page]
   [juxt.dirwatch :refer [watch-dir close-watcher]]
   ))

(def pages
  (atom {}))

(def template-suffix ".fab")
(def template-suffix-regex (re-pattern (str "#*[.]" template-suffix "$")))

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
        tmpl-ns (if form-ns form-ns
                    (symbol (str "tmp-ns." (Math/abs (hash parsed)))))
        evaluated  (read/eval-with-errors
                    parsed tmpl-ns html/validate-element)
        page-meta (read/eval-in-ns 'metadata tmpl-ns)
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
  ([] (render-template-files (get-template-files "content" template-suffix))))



(defn update-page-map [old-map page-name page-contents]
  (if (= ::read/parse-error page-contents)
    (do
      (println "Parse error detected, skipping")
      old-map)
    (assoc old-map page-name {:data page-contents
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

(defn load-deps []
  (require '[site.fabricate.prototype.html :refer :all]))

(defn draft
  ([]
   (do
     (load-deps)
     (doseq [fp (get-template-files "content" template-suffix)]
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
         (apply concat files (map #(get-template-files % template-suffix) dirs))]
     (render-template-files all-files))))
