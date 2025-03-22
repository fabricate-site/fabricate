(ns site.fabricate.source
  "Default sources for Fabricate"
  (:require [site.fabricate.api :as api]
            [site.fabricate.prototype.source.clojure :as clj]
            [site.fabricate.prototype.source.fabricate :as fabricate]
            [babashka.fs :as fs])
  (:import [java.time ZonedDateTime ZoneId]))

(def defaults
  "Default options for sources."
  {::location (fs/file (System/getProperty "user.dir") "docs")})

(defn- filetime->zdt
  [ft]
  (ZonedDateTime/ofInstant (fs/file-time->instant ft) (ZoneId/systemDefault)))

(defn file-times
  "Return a map with two ZonedDateTimes representing when the file was created and modified."
  [f]
  {::created  (filetime->zdt (fs/creation-time f))
   ::modified (filetime->zdt (fs/last-modified-time f))})

(defmethod api/collect "**/*.clj"
  [src
   {source-location ::location
    :or {source-location (:site.fabricate.source/location defaults)}
    :as opts}]
  (let [clj-files (fs/glob source-location src)]
    (mapv (fn clj-path->entry [p]
            (merge (file-times p)
                   {::format     :clojure/file
                    ::location   (fs/file p)
                    ::api/source src
                    :site.fabricate.document/format :hiccup/article
                    :site.fabricate.page/format :html
                    ::file       (fs/file p)}))
          clj-files)))

(defmethod api/collect "**/*.fab"
  [src
   {source-location ::location
    :or {source-location (::location defaults)}
    :as opts}]
  (let [fabricate-templates (fs/glob source-location src)]
    (mapv (fn template->entry [p]
            (merge (file-times p)
                   {::format     ::fabricate/v0
                    ::file       (fs/file p)
                    ::api/source src
                    ::location   (fs/file p)
                    :site.fabricate.document/format :hiccup
                    :site.fabricate.page/format :html}))
          fabricate-templates)))
