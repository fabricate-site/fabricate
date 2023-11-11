(ns site.fabricate.prototype.api
  "Fabricate's public API.

  This contains the core set of operations that Fabricate uses to produce a website from input files."
  (:require [site.fabricate.prototype.read :as read]
            [site.fabricate.prototype.page :as page]
            [site.fabricate.prototype.html :as html]
            [malli.core :as m]
            [hiccup.page]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [babashka.fs :as fs])
  (:import [java.io File]
           [java.net URI]))

(comment
  (def entry-schema
    (m/schema [:map
               [:site.fabricate/input-file [:fn #(instance? java.io.File %)]]
               [:site.fabricate/output-file [:fn #(instance? java.io.File %)]]
               [:page/title :string]])))

(spec/def :site.fabricate/input-file #(instance? File %))
(spec/def :site.fabricate/output-file #(instance? File %))

(spec/def :site.fabricate/entry
  (spec/keys :req [:site.fabricate/input-file]
             :opt [:site.fabricate/output-file]))

(def patterns
  {"**.fab" {:site.fabricate.page/input-format :fabricate/v0,
             :site.fabricate.page/format :hiccup,
             :site.fabricate.page/outputs
               [{:site.fabricate.page/output-format :html,
                 :site.fabricate.page/output-location "docs"}]}})

(defn- plan-pattern-entries
  ([[pattern entry-data] input-dir]
   (for [matched-file (fs/glob input-dir pattern)
         output (:site.fabricate.page/outputs entry-data)]
     (-> entry-data
         (dissoc :site.fabricate.page/outputs)
         (assoc :site.fabricate.page/input-file (fs/file matched-file)
                :site.fabricate.page/output output
                :site.fabricate.page/match-pattern pattern)))))

(defn plan
  [{:keys [patterns input-dir], :as settings}]
  (into [] (mapcat #(plan-pattern-entries % input-dir)) patterns))

;; not enforcing a spec on a multimethod seems like the best way of keeping
;; it open for extension, and keeps the API simpler.
(defmulti assemble
  "Generate structured content from the entry's input file."
  {:term/definition
     {:source (URI. "https://www.merriam-webster.com/dictionary/assemble"),
      :definition "to fit together the parts of"}}
  (fn [entry] [(:site.fabricate.page/input-format entry)
               (:site.fabricate.page/format entry)]))

(defn fabricate-v0->hiccup
  "Generate a Hiccup representation of the page by evaluating the parsed Fabricate template of the page contents."
  [{:keys [site.fabricate.page/input-file], :as entry}]
  (let [parsed-page (read/parse (slurp input-file))
        evaluated-page (read/eval-all parsed-page)
        page-metadata (page/lift-metadata evaluated-page {})
        hiccup-page [:html (page/doc-header page-metadata)
                     [:body
                      [:main
                       (apply conj
                              [:article {:lang "en-us"}]
                              (page/parse-paragraphs evaluated-page))]
                      [:footer [:div [:a {:href "/"} "Home"]]]]]]
    (assoc entry :site.fabricate.page/data hiccup-page)))

(defmethod assemble [:fabricate/v0 :hiccup]
  [entry]
  (fabricate-v0->hiccup entry))

;; (def assemble-index nil)

;; (defmethod assemble "index.html" [entry] (assemble-index entry))

(defn- output-path
  [input-file output-location]
  (cond (fs/directory? output-location)
        (fs/file (str output-location "/" (fs/file-name input-file)))
        (instance? java.io.File output-location) output-location))

(defmulti produce!
  "Generate output from an entry's page data and write it to the designated output file."
  {:term/definition
   {:source (URI. "https://www.merriam-webster.com/dictionary/produce"),
    :definition
    "to make available for public exhibition or dissemination; to cause to have existence or to happen; to give being, form, or shape to; to compose, create, or bring out by intellectual or physical effort; to bear, make, or yield something"}}
  (fn [entry] [(:site.fabricate.page/format entry)
               (get-in entry
                       [:site.fabricate.page/output
                        :site.fabricate.page/output-format])]))

(defn write-hiccup-html!
  "Generate HTML from Hiccup data and write it to the given file."
  [hiccup-page-data output-file]
  (spit output-file (hiccup.page/html5 hiccup-page-data)))

(defmethod produce! [:hiccup :html]
  [{:keys [site.fabricate.page/data site.fabricate.page/output
           site.fabricate.page/input-file],
    :as entry}]
  (let [output-file (fs/file (str (output-path
                                   input-file
                                   (:site.fabricate.page/output-location
                                    output))
                                  ".html"))]
    (write-hiccup-html! data output-file)
    (-> entry
        (dissoc :site.fabricate.page/output)
        (assoc :site.fabricate.page/output-file output-file
               :site.fabricate.page/output-format :html))))

(comment
  (produce! (assemble (first (plan {:patterns patterns, :input-dir "pages"}))))
  (meta #'assemble)
  (meta assemble))
