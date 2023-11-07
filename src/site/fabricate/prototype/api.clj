(ns site.fabricate.prototype.api
  "Fabricate's public API.

  This contains the core set of operations that Fabricate uses to produce a website from input files."
  (:require [site.fabricate.prototype.read :as read]
            [site.fabricate.prototype.page :as page]
            [site.fabricate.prototype.html :as html]
            [malli.core :as m]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [babashka.fs :as fs]))

(comment
  (def entry-schema
    (m/schema [:map
               [:site.fabricate/input-file [:fn #(instance? java.io.File %)]]
               [:site.fabricate/output-file [:fn #(instance? java.io.File %)]]
               [:page/title :string]])))

(spec/def :site.fabricate/input-file #(instance? java.io.File %))
(spec/def :site.fabricate/output-file #(instance? java.io.File %))

(spec/def :site.fabricate/entry
  (spec/keys :req [:site.fabricate/input-file]
             :opt [:site.fabricate/output-file]))


(def patterns)

(def patterns
  {;; "docs/**.md"
   ;; {:input/format :markdown,
   ;;  :page/format :hiccup,
   ;;  :outputs [{:output/format :pdf, :output/location "pdfs/"}
   ;;            {:output/format :html, :output/location "html/"}]},
   "**.fab" {:input/format :fabricate/v1,
             :page/format :hiccup,
             :outputs [{:output/format :html, :output/location "docs"}]}
   ;; "**.docx" {:input/format :word/docx}
   ;; "./obsidian/website/*.md" {:input/format :markdown}
   })

(defn- output-path
  [input-file output-location]
  (cond (fs/directory? output-location)
        (fs/file (str output-location "/" (fs/file-name input-file)))
        (instance? java.io.File output-location) output-location))

(defn- plan-pattern-entries
  ([[pattern entry-data] input-dir]
   (for [matched-file (fs/glob input-dir pattern)
         output (:outputs entry-data)]
     (let [input-file (fs/file matched-file)]
       (-> entry-data
           (dissoc :outputs)
           (assoc :site.fabricate/input-file input-file
                  :site.fabricate/output output
                  :match/pattern pattern))))))

(defn plan
  [{:keys [patterns input-dir], :as settings}]
  (into [] (mapcat #(plan-pattern-entries % input-dir)) patterns))

;; not enforcing a spec on a multimethod seems like the best way of keeping
;; it open for extension, and keeps the API simpler.
(defmulti assemble
  "Assemble the given entry by generating structured content from the entry's input file"
  (fn [entry] [(:input/format entry) (:page/format entry)]))

(defn fabricate-v1->hiccup
  [{:keys [site.fabricate/input-file], :as entry}]
  (let [parsed-page (read/parse (slurp input-file))
        evaluated-page (read/eval-all parsed-page)]
    (assoc entry :page/data evaluated-page)))

(defmethod assemble [:fabricate/v1 :hiccup]
  [entry]
  (fabricate-v1->hiccup entry))

(def assemble-index nil)

(defmethod assemble "index.html" [entry] (assemble-index entry))

(comment
  (plan {:input-dir "pages", :patterns patterns}))
