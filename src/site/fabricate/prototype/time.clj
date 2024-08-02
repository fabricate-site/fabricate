(ns site.fabricate.prototype.time
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.java.shell :refer [sh]])
  (:import [java.time ZonedDateTime ZoneId]
           [java.time.format DateTimeFormatter]))


(defn git-modified-times
  "Return the list of times a file was updated in Git.

  Returns nil if not found."
  [fp]
  (let [{:keys [out err]} (sh "git"
                              "log"          "--follow"
                              "--format=%ad" "--date"
                              "iso-strict"   (str fp))]
    (when (not-empty out)
      (mapv #(ZonedDateTime/parse %) (str/split out #"\n")))))

(defn filetime->zdt
  [ft]
  (ZonedDateTime/ofInstant (fs/file-time->instant ft) (ZoneId/systemDefault)))


(defn git-created
  "Return a ZonedDateTime representing the time a given file was first tracked by Git.

  Returns nil if git is unavailable."
  [fp]
  (first (git-modified-times fp)))

(defn git-modified
  "Return a ZonedDateTime representing the time a file was most recently modified by Git.

  Returns nil if git is unavailable."
  [fp]
  (peek (git-modified-times fp)))

(defn file-created
  "Returns the time the file was created according to Git, falling back to the filesystem if the file is untracked or Git encounters an error."
  [fp]
  (or (git-created fp) (filetime->zdt (fs/creation-time fp))))

(defn file-modified
  "Returns the time the file was created according to Git, falling back to the filesystem if the file is untracked or Git encounters an error."
  [fp]
  (or (git-modified fp) (filetime->zdt (fs/last-modified-time fp))))
