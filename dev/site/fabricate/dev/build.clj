(ns site.fabricate.dev.build
  "Build namespace for generating Fabricate's own documentation."
  (:require [site.fabricate.prototype.api :as api]
            [site.fabricate.dev.styles :as styles]
            [site.fabricate.prototype.time :as time]
            [site.fabricate.prototype.read :as read]
            [site.fabricate.prototype.page :as page]
            [site.fabricate.prototype.html :as html]
            [garden.core :as garden]
            [garden.stylesheet :refer [at-import]]
            [babashka.fs :as fs]
            [hiccup.page]
            [hiccup.core :as hiccup]
            [clojure.string :as str]
            [clojure.java.io :as io]))


(defn create-dir-recursive
  [target-dir]
  (->> (fs/path (fs/cwd) target-dir)
       (fs/relativize (fs/cwd))
       fs/components
       (reduce (fn [paths path] (conj paths (fs/path (peek paths) path))) [])
       (filter #(not (fs/exists? %)))
       (run! fs/create-dir)))

(defn create-dir? [d] (when-not (fs/exists? d) (create-dir-recursive d)))

(defn create-publish-dirs!
  [{:keys [site.fabricate.page/publish-dir], :as options}]
  (let [css-dir (fs/path publish-dir "css")
        fonts-dir (fs/path publish-dir "fonts")]
    (run! create-dir? [publish-dir css-dir fonts-dir])))

(defn get-css!
  [{:keys [site.fabricate.page/publish-dir], :as options}]
  (let
    [remedy
       {:file (fs/file (fs/path publish-dir "css" "remedy.css")),
        :url
          "https://raw.githubusercontent.com/jensimmons/cssremedy/6590d9630bdd324469620636d85b7ea3753e9a7b/css/remedy.css"}
     normalize
       {:file (fs/file (fs/path publish-dir "css" "normalize.css")),
        :url "https://unpkg.com/@csstools/normalize.css@12.1.1/normalize.css"}]
    (doseq [{:keys [file url]} [normalize remedy]]
      (when-not (fs/exists? file) (io/copy url file)))))

(defn copy-fonts!
  [{:keys [site.fabricate.page/publish-dir], :as options}]
  (let [font-dir (System/getProperty "user.font-dir")
        fonts []]
    (doseq [{:keys [src file]} fonts]
      (when-not (fs/exists? file) (fs/copy src file)))))


(def options {:site.fabricate.page/publish-dir "docs"})

(def setup-tasks [create-publish-dirs! get-css! copy-fonts!])

(defmethod api/collect "pages/**.fab"
  [src options]
  (mapv (fn path->entry [p]
          {:site.fabricate.source/format :site.fabricate.read/v0,
           :site.fabricate.document/format :hiccup,
           :site.fabricate.source/location (fs/file p),
           :site.fabricate.entry/source src,
           :site.fabricate.source/created (time/file-created p),
           :site.fabricate.source/modified (time/file-modified p),
           :site.fabricate.page/outputs
             [{:site.fabricate.page/format :html,
               :site.fabricate.page/location
                 (fs/file (:site.fabricate.page/publish-dir options))}]})
    (fs/glob (System/getProperty "user.dir") src)))


;; example of single-file handling; conflict resolution can be handled
;; separately if there's overlap.

(defmethod api/collect "README.md.fab"
  [src options]
  [{:site.fabricate.source/location (fs/file src),
    :site.fabricate.entry/source src,
    :site.fabricate.source/created (time/file-created src),
    :site.fabricate.source/modified (time/file-modified src),
    :site.fabricate.page/title "Fabricate: README",
    :site.fabricate.source/format :site.fabricate.markdown/v0,
    :site.fabricate.document/format :markdown,
    :site.fabricate.page/outputs
      [{:site.fabricate.page/format :markdown,
        :site.fabricate.page/location
          (fs/file (str (:site.fabricate.page/publish-dir options)
                        "/README.md"))}]}])


(defn fabricate-v0->hiccup
  "Generate a Hiccup representation of the page by evaluating the parsed Fabricate template of the page contents."
  [entry]
  (let [parsed-page (read/parse (slurp (:site.fabricate.source/location entry)))
        evaluated-page (read/eval-all parsed-page)
        page-metadata (update (page/lift-metadata evaluated-page
                                                  (last (read/get-metadata
                                                         parsed-page)))
                              :page-style
                              eval)
        hiccup-page [:html (page/doc-header page-metadata)
                     [:body
                      [:main
                       (apply conj
                              [:article {:lang "en-us"}]
                              (page/parse-paragraphs evaluated-page))]
                      [:footer [:div [:a {:href "/"} "Home"]]]]]]
    (assoc entry
           :site.fabricate.document/data hiccup-page
           :site.fabricate.page/title (:title page-metadata))))

(defmethod api/assemble [:site.fabricate.read/v0 :hiccup]
  [entry]
  (fabricate-v0->hiccup entry))

(defmethod api/assemble [:site.fabricate.markdown/v0 :markdown]
  [entry]
  (assoc entry
         :site.fabricate.document/data (slurp (:site.fabricate.source/location
                                               entry))))

;; (def assemble-index nil)

;; (defmethod assemble "index.html" [entry] (assemble-index entry))


(defn write-hiccup-html!
  "Generate HTML from Hiccup data and write it to the given file."
  [hiccup-page-data output-file]
  (let [parent-dir (fs/parent output-file)]
    (create-dir? parent-dir)
    (spit output-file (hiccup/html hiccup-page-data))))

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

(defmethod api/produce! [:hiccup :html]
  [entry]
  (let [output-file (fs/file (str (output-path
                                   (fs/strip-ext
                                    (fs/strip-ext
                                     (:site.fabricate.source/location
                                      entry)))
                                   (:site.fabricate.page/location entry))
                                  ".html"))]
    (write-hiccup-html! (:site.fabricate.document/data entry) output-file)
    (-> entry
        (assoc :site.fabricate.page/output output-file
               :site.fabricate.page/format :html))))

(defmethod api/produce! [:markdown :markdown]
  [entry]
  (let [output-file (fs/file (output-path
                              (:site.fabricate.source/location entry)
                              (:site.fabricate.page/location entry)))]
    (spit output-file (:site.fabricate.document/data entry))
    (assoc entry :site.fabricate.page/output output-file)))

(comment
  (require '[http.server :as server])
  (defonce srv
    (server/start {:cors-allow-headers nil,
                   :dir (str (fs/path (fs/cwd) "docs")),
                   :port 8002,
                   :no-cache true}))
  (let [entries (api/plan! setup-tasks options)
        assembled-entries (api/combine []
                                       {:site.fabricate.api/options options,
                                        :site.fabricate.api/entries entries})]
    (api/construct! [] assembled-entries))
  (garden/css styles/docs)
  (run! fs/delete (fs/glob "docs" "**.html")))
