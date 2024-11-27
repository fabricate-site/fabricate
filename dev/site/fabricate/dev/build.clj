(ns site.fabricate.dev.build
  "Build namespace for generating Fabricate's own documentation."
  (:require [site.fabricate.api :as api]
            [site.fabricate.dev.styles :as styles]
            [site.fabricate.dev.elements :as elements]
            [site.fabricate.prototype.time :as time]
            [site.fabricate.prototype.read :as read]
            [site.fabricate.prototype.read.grammar :as grammar]
            [site.fabricate.prototype.hiccup :as hiccup]
            [site.fabricate.prototype.html :as html]
            [garden.core :as garden]
            [garden.stylesheet :refer [at-import]]
            [rewrite-clj.zip :as z]
            [site.fabricate.adorn :as adorn]
            [babashka.fs :as fs]
            [hiccup.page]
            [hiccup.core]
            [dev.onionpancakes.chassis.core :as c]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [http.server :as server]))




(defn simple-expr
  "Takes a Clojure form and yields a string with the Fabricate template expression for that form."
  {:malli/schema [:=> [:cat :any :map] :string]}
  [form {:keys [ctrl-char format-fn] :or {format-fn str ctrl-char ""}}]
  (let [[start end] grammar/delimiters]
    (str start ctrl-char (format-fn form) end)))

(defn create-dir-recursive
  [target-dir]
  (let [absolute-path? (fs/absolute? target-dir)
        target-dir     (if (fs/relative? target-dir)
                         (fs/relativize (fs/cwd) (fs/path (fs/cwd) target-dir))
                         target-dir)]
    (->> target-dir
         fs/components
         (reduce (fn [paths path]
                   (conj paths
                         (let [next-path (fs/path (peek paths) path)]
                           (if absolute-path?
                             (fs/absolutize (str fs/file-separator next-path))
                             next-path))))
                 [])
         (filter #(not (fs/exists? %)))
         (run! fs/create-dir))))

(defn create-dir? [d] (when-not (fs/exists? d) (create-dir-recursive d)))

(defn create-publish-dirs!
  [{:keys [site.fabricate.api/options] :as site}]
  (let [{:keys [site.fabricate.page/publish-dir]} options
        css-dir   (fs/path publish-dir "css")
        fonts-dir (fs/path publish-dir "fonts")]
    (run! create-dir? [publish-dir css-dir fonts-dir])
    site))

(defn get-css!
  [{:keys [site.fabricate.api/options] :as site}]
  (let
    [{:keys [site.fabricate.page/publish-dir]} options
     remedy
     {:file (fs/file (fs/path publish-dir "css" "remedy.css"))
      :url
      "https://raw.githubusercontent.com/jensimmons/cssremedy/6590d9630bdd324469620636d85b7ea3753e9a7b/css/remedy.css"}
     normalize
     {:file (fs/file (fs/path publish-dir "css" "normalize.css"))
      :url  "https://unpkg.com/@csstools/normalize.css@12.1.1/normalize.css"}
     patterns {:file (fs/file (fs/path publish-dir "css" "patterns.css"))
               :url  "https://iros.github.io/patternfills/patterns.css"}]
    (doseq [{:keys [file url]} [normalize remedy patterns]]
      (when-not (fs/exists? file) (spit file (slurp url))))
    site))

(defn copy-fonts!
  [{:keys [site.fabricate.api/options] :as site}]
  (let [{:keys [site.fabricate.page/publish-dir]} options
        font-dir (System/getProperty "user.font-dir")
        fonts    (reduce
                  (fn [fonts path] (conj fonts {:src "" :file ""}))
                  [{:src
                    (fs/file
                     font-dir
                     "CommitMono-stdV142-design/CommitMono VariableFont.woff2")
                    :file "docs/fonts/CommitMono VariableFont.woff2"}]
                  (fs/glob (fs/path font-dir "Lapidar0.3") "*.woff2"))]
    (doseq [{:keys [src file]} fonts]
      (when-not (fs/exists? file) (fs/copy src file)))
    site))

(def options
  "Options for building Fabricate's own documentation."
  (let [d "docs"]
    {:site.fabricate.page/publish-dir d
     :site.fabricate.dev.build/server {:port 7779 :dir d}}))


(defmethod api/collect "pages/**.fab"
  [src options]
  (mapv (fn path->entry [p]
          {:site.fabricate.source/format   :site.fabricate.read/v0
           :site.fabricate.document/format :hiccup
           :site.fabricate.source/location (fs/file p)
           :site.fabricate.entry/source    src
           :site.fabricate.source/created  (time/file-created p)
           :site.fabricate.source/modified (time/file-modified p)
           ;; multi-outputs are superfluous; this should be replaced with a
           ;; call to mapcat
           :site.fabricate.page/outputs    [{:site.fabricate.page/format :html
                                             :site.fabricate.page/location
                                             (fs/file
                                              (:site.fabricate.page/publish-dir
                                               options))}]})
        (fs/glob (System/getProperty "user.dir") src)))


;; example of single-file handling; conflict resolution can be handled
;; separately if there's overlap.

(defmethod api/collect "README.md.fab"
  [src options]
  [{:site.fabricate.source/location (fs/file src)
    :site.fabricate.entry/source    src
    :site.fabricate.source/created  (time/file-created src)
    :site.fabricate.source/modified (time/file-modified src)
    :site.fabricate.page/title      "Fabricate: README"
    :site.fabricate.source/format   :site.fabricate.markdown/v0
    :site.fabricate.document/format :markdown
    :site.fabricate.page/outputs    [{:site.fabricate.page/format :markdown
                                      :site.fabricate.page/location
                                      (fs/file
                                       (str (:site.fabricate.page/publish-dir
                                             options)
                                            "/README.md"))}]}])


(defn fabricate-v0->hiccup
  "Generate a Hiccup representation of the page by evaluating the parsed Fabricate template of the page contents."
  [entry]
  (let [parsed-page    (read/parse (slurp (:site.fabricate.source/location
                                           entry)))
        evaluated-page (read/eval-all parsed-page)
        page-metadata  (hiccup/lift-metadata evaluated-page
                                             (let [m (:metadata
                                                      (meta evaluated-page))]
                                               ;; TODO: better handling of
                                               ;; unbound metadata vars
                                               (if (map? m) m {})))
        hiccup-page    [:html (hiccup/doc-header page-metadata)
                        [:body
                         [:main
                          (apply conj
                                 [:article {:lang "en-us"}]
                                 (hiccup/parse-paragraphs evaluated-page))]
                         (elements/footer)
                         #_[:footer [:div [:a {:href "/"} "Home"]]]]]]
    (assoc entry
           :site.fabricate.document/data hiccup-page
           :site.fabricate.page/title    (:title page-metadata))))

(defmethod api/build [:site.fabricate.read/v0 :hiccup]
  ([{loc :site.fabricate.source/location :as entry} _opts]
   (try (fabricate-v0->hiccup entry)
        (catch Exception ex
          (throw (ex-info (ex-message ex)
                          (assoc (Throwable->map ex
                                                 :site.fabricate.source/location
                                                 loc))))))))

(defmethod api/build [:site.fabricate.markdown/v0 :markdown]
  ([entry _opts]
   (assoc entry
          :site.fabricate.document/data
          (slurp (:site.fabricate.source/location entry)))))

;; (def assemble-index nil)

;; (defmethod assemble "index.html" [entry] (assemble-index entry))


(defn write-hiccup-html!
  "Generate HTML from Hiccup data and write it to the given file."
  [hiccup-page-data output-file]
  (let [parent-dir (fs/parent output-file)]
    (create-dir? parent-dir)
    (spit output-file (c/html [c/doctype-html5 hiccup-page-data]))))

(defn subpath
  ([dir p] (apply fs/path (drop 1 (fs/components (fs/relativize dir p)))))
  ([p] (subpath (fs/cwd) p)))

(comment
  (fs/parent "docs/README.md")
  (subpath "docs/path/to/some/file"))

(defn output-path
  [input-file output-location]
  (cond (fs/directory? output-location) (fs/file (fs/path output-location
                                                          (subpath input-file)))
        (instance? java.io.File output-location) output-location))

(defn hiccup->html
  [entry _opts]
  (let [output-file (fs/file (str (output-path
                                   (fs/strip-ext
                                    (fs/strip-ext
                                     (:site.fabricate.source/location entry)))
                                   (:site.fabricate.page/location entry))
                                  ".html"))]
    (write-hiccup-html! (:site.fabricate.document/data entry) output-file)
    (assert (fs/exists? output-file))
    (-> entry
        (assoc :site.fabricate.page/output output-file
               :site.fabricate.page/format :html))))

(defmethod api/produce! [:hiccup :html] [entry opts] (hiccup->html entry opts))

(defmethod api/produce! [:markdown :markdown]
  [entry _opts]
  (let [output-file (fs/file (output-path
                              (:site.fabricate.source/location entry)
                              (:site.fabricate.page/location entry)))]
    (spit output-file (:site.fabricate.document/data entry))
    (assoc entry :site.fabricate.page/output output-file)))

(defonce file-server (atom nil))

(defn launch-server!
  [{:keys [site.fabricate.api/options] :as site}]
  (when (nil? @file-server)
    (reset! file-server (server/start (get options ::server))))
  site)


(defn shutdown-server!
  [& _args]
  (when-not (nil? @file-server) (swap! file-server #(do (server/stop %) nil))))


(def setup-tasks [create-publish-dirs! get-css! copy-fonts! launch-server!])

(comment
  ;; it's hard to beat this simplicity. also, a point in favor of the
  ;; "return a modified site with modified options" implementation:
  ;; potentially storing a reference to a server or other stateful
  ;; component
  @file-server
  (do (->> {:site.fabricate.api/options options}
           (api/plan! setup-tasks)
           (api/assemble [])
           (api/construct! []))
      :done)
  (run! fs/delete (fs/glob "docs" "**.html"))
  (.getMethodTable api/produce!))


(comment
  (names)
  (str/split (str (symbol :site.fabricate.document/data)) #"\.")
  (name :abc/xyz)
  clojure.string/split
  (run! (fn [[_ v]] (clojure.pprint/pprint [v (:doc (meta v))]))
        (ns-publics (find-ns 'site.fabricate.api))))
