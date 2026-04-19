(ns site.fabricate.source
  "Default sources for Fabricate"
  (:require [site.fabricate.api :as api]
            [site.fabricate.prototype.source.clojure :as clj]
            [site.fabricate.prototype.source.fabricate :as fabricate]
            [babashka.fs :as fs])
  (:import [java.time ZonedDateTime ZoneId]))

(def defaults
  "Default options for sources."
  {:site.fabricate.source/location (fs/file (System/getProperty "user.dir")
                                            "docs")})

(defn- filetime->zdt
  [ft]
  (ZonedDateTime/ofInstant (fs/file-time->instant ft) (ZoneId/systemDefault)))

(defn path->entry-map
  "Return components of an entry map with format-independent metadata"
  {:malli/schema [:-> [:fn fs/exists?] :map]}
  [p]
  {::directory (fs/parent (fs/canonicalize (fs/absolutize p)))
   ::location  (fs/file (fs/canonicalize (fs/absolutize p)))
   ::created   (filetime->zdt (fs/creation-time p))
   ::modified  (filetime->zdt (fs/last-modified-time p))})

(defmethod api/collect "**/*.clj"
  [src
   {source-location :site.fabricate.source/location
    :or {source-location (:site.fabricate.source/location defaults)}
    :as opts}]
  (let [clj-files (fs/glob source-location src)]
    (mapv (fn clj-path->entry [p]
            (merge (path->entry-map p)
                   {:site.fabricate.source/format   :clojure/file
                    :site.fabricate.api/source      src
                    :site.fabricate.document/format :hiccup/article
                    :site.fabricate.page/format     :html}))
          clj-files)))

(defmethod api/collect "**/*.fab"
  [src
   {source-location :site.fabricate.source/location
    :or {source-location (:site.fabricate.source/location defaults)}
    :as opts}]
  (let [fabricate-templates (fs/glob source-location src)]
    (mapv (fn template->entry [p]
            (merge (path->entry-map p)
                   {:site.fabricate.source/format
                    :site.fabricate.prototype.source.fabricate/v0
                    :site.fabricate.api/source src
                    :site.fabricate.document/format :hiccup
                    :site.fabricate.page/format :html}))
          fabricate-templates)))
