(ns site.fabricate.dev.build
  "Build namespace for generating Fabricate's own documentation."
  (:require [site.fabricate.prototype.api :as api]
            [site.fabricate.dev.styles :as styles]
            [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.java.io :as io]))


(defn create-dir-recursive
  [target-dir]
  (->> (str/split (str (fs/relativize (fs/cwd) (fs/path (fs/cwd) target-dir)))
                  (re-pattern fs/file-separator))
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



(comment
  (api/plan! setup-tasks options)
  (fs/glob (System/getProperty "user.font-dir") "**.{woff,woff2}")
  (fs/file (fs/path "docs" "css" "basic")))
