(ns site.fabricate.api
  "Fabricate's public API.

  This contains the core set of operations that Fabricate uses to produce a website from input files."
  (:require [site.fabricate.prototype.schema :as s]
            [malli.core :as m]
            [malli.util :as mu]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as spec]
            [babashka.fs :as fs])
  (:import [java.io File]
           [java.net URI]
           [java.time ZonedDateTime Instant ZoneId]))


(def glossary
  "Key terms used by Fabricate."
  [{:doc  "A source file used by Fabricate. May be a file, or a URL."
    :term :site.fabricate.source/location
    :type [:or :string [:fn fs/exists?]]}
   {:doc
    "The source from which input entries are derived. collect dispatches on ::source"
    :term :site.fabricate.api/source
    :type :any}
   {:doc
    "A map representing a component of a page before, during, and after the assemble and produce operations. One source may produce multiple entries."
    :term :site.fabricate.api/entry
    :type :map}
   {:doc  "The file where a page was published."
    :term :site.fabricate.page/output
    :type [:fn fs/exists?]}
   {:doc  "Clojure value with the contents of a document."
    :term :site.fabricate.document/data
    :type :any}
   {:doc  "Clojure metadata map with a document's metadata."
    :term :site.fabricate.document/metadata
    :type :map}
   {:doc
    "The specification or type of the document data generated by `assemble`. An entry has a single document format."
    :term :site.fabricate.document/format
    :type :keyword}
   {:doc
    "The specification or type of the file format from which page data is generated by passing the file's contents to `assemble`. An entry has a single source format."
    :term :site.fabricate.source/format
    :type :keyword}
   {:doc
    "The specification or type of the file format that page data is converted to by produce before being written to a file. An entry has one output format."
    :term :site.fabricate.page/format
    :type :keyword}
   {:doc  "The address at which a page will be accessed after publication."
    :term :site.fabricate.page/uri
    :type [:fn uri?]}
   {:doc "The title of a page." :term :site.fabricate.page/title :type :string}
   {:doc  "A page's permanent, canonical URL."
    :term :site.fabricate.page/permalink
    :type [:fn uri?]}
   {:doc  "A brief description of a page's contents."
    :term :site.fabricate.page/description
    :type :string}
   {:doc  "The author of a given page."
    :term :site.fabricate.page/author
    :type :string}
   {:doc  "The language a page is written in."
    :term :site.fabricate.page/language
    :type :string}
   {:doc  "The locale of a page."
    :term :site.fabricate.page/locale
    :type :string}
   {:doc  "The URL of an image accompanying or representing a page."
    :term :site.fabricate.page/image
    :type [:fn uri?]}
   {:doc  "The Open Graph Protocol type of a page."
    :term :me.ogp/type
    :type :string}
   {:doc  "The datetime (ISO 8601-compatible) a page was first published."
    :term :site.fabricate.page/published-time
    :type :time/zoned-date-time}
   {:term :site.fabricate.page/publish-dir
    :doc  "The directory where Fabricate pages should be published."
    :type [:fn fs/exists?]}
   {:doc  "The datetime (ISO 8601-compatible) a source was first created."
    :term :site.fabricate.source/created
    :type :time/zoned-date-time}
   {:doc
    "The datetime (ISO 8601-compatible) a source was most recently updated."
    :term :site.fabricate.source/modified
    :type :time/zoned-date-time}
   {:doc  "The datetime (ISO 8601-compatible) a page was most recently updated."
    :term :site.fabricate.page/modified-time
    :type :time/zoned-date-time}
   {:doc
    "Tags and labels describing the content of a page. May be strings, Clojure keywords, or Clojure symbols."
    :term :site.fabricate.page/tags
    :type [:* [:or :string :keyword :symbol]]}
   {:doc
    "The unique identifier for a document. May be a string, UUID, symbol, or Clojure namespace."
    :term :site.fabricate.entry/id
    :type [:or :string [:fn uuid?] :symbol :namespace]}
   {:doc  "The Clojure namespace of an entry."
    :term :site.fabricate.entry/namespace
    :type :namespace}])

;; register schema components and bind them to a registry var
(def registry
  "Registry for the Malli schemas defining aspects of Fabricate's API."
  (into {}
        (for [{:keys [term doc] :as d} glossary]
          (when-not (= :site.fabricate.api/entry term)
            (let [schema
                  (mu/update-properties (m/schema (:type d)) assoc :doc doc)]
              (s/register! term schema)
              [term schema])))))


(def entry-schema
  "Malli schema describing entries."
  (mu/required-keys
   (mu/optional-keys
    (m/schema [:map :site.fabricate.api/source :site.fabricate.source/location
               :site.fabricate.page/output :site.fabricate.document/data
               :site.fabricate.document/format :site.fabricate.source/format
               :site.fabricate.page/format :site.fabricate.page/uri
               :site.fabricate.page/title :site.fabricate.page/permalink
               :site.fabricate.page/description :site.fabricate.page/author
               :site.fabricate.page/language :site.fabricate.page/locale
               :site.fabricate.page/image :me.ogp/type
               :site.fabricate.page/published-time
               #_:site.fabricate.document/metadata
               :site.fabricate.page/publish-dir :site.fabricate.source/created
               :site.fabricate.source/modified
               :site.fabricate.page/modified-time :site.fabricate.page/tags
               :site.fabricate.entry/id :site.fabricate.entry/namespace]))
   ;; entries ultimately have to come from somewhere.
   [:site.fabricate.api/source :site.fabricate.source/location
    :site.fabricate.source/format]))


(defn collect-dispatch
  "Equivalent to a multi-arg version of `clojure.core/identity` for a source."
  {:malli/schema (m/schema [:=> [:cat :site.fabricate.api/source :map]
                            :site.fabricate.api/source])
   :private      true}
  [src options]
  src)

(defmulti collect
  "Generate the input entries from a source."
  {:malli/schema (:malli/schema (meta #'collect-dispatch))}
  collect-dispatch)

(def site-schema
  "Malli schema describing the contents of a Fabricate site.

A site is the primary map passed between the 3 core API functions: plan!, assemble, and construct!"
  (m/schema [:map [:site.fabricate.api/entries [:* entry-schema]]
             [:site.fabricate.api/options :map]]))

(def site-fn-schema
  "Function schema for functions that operate on a site"
  (m/schema [:schema
             {:registry {::task-list [:or [:* [:schema [:ref ::site-fn]]]
                                      [:map-of [:schema [:ref ::site-fn]]
                                       [:schema [:ref ::site-fn]]]]
                         ::site-fn   [:=>
                                      [:cat [:schema [:ref ::task-list]]
                                       [:schema site-schema]]
                                      [:schema site-schema]]}} ::site-fn]))


;; question: should this actually be exactly the same signature /
;; implementation as the other functions in the API? Making it different
;; - by simply executing each setup task for its side effects rather than
;; for returning an updated site - encourages keeping the initial setup
;; simpler, and makes the API somewhat more orthogonal. I think this
;; question is better answered through use while the API continues to
;; stabilize.

(defn plan!
  "Execute all the given `setup-tasks`, then `collect` the list of entries from each source.

  This list of entries will be appended to any entries passed in as a component of the `site` argument."
  {:malli/schema site-fn-schema}
  [setup-tasks
   {:keys [site.fabricate.api/entries site.fabricate.api/options]
    :or   {entries []}
    :as   site}]
  (let [post-setup-site   (reduce (fn [site task] (task site)) site setup-tasks)
        collected-entries (vec (for [[source _] (.getMethodTable collect)
                                     entry-data (collect source options)
                                     ;; this is superfluous, but is
                                     ;; retained for backwards
                                     ;; compatibility
                                     output     (:site.fabricate.page/outputs
                                                 entry-data
                                                 [:default])]
                                 (if (= output :default)
                                   entry-data
                                   (-> entry-data
                                       (dissoc :site.fabricate.page/outputs)
                                       (merge output)))))]
    (update post-setup-site
            :site.fabricate.api/entries
            (fn [es] (reduce conj es collected-entries)))))


(defn build-dispatch
  "Return the source and document formats for an entry."
  {:malli/schema (m/schema [:=> [:cat entry-schema :map]
                            [:tuple :site.fabricate.source/format
                             :site.fabricate.document/format]])}
  [entry options]
  [(:site.fabricate.source/format entry)
   (:site.fabricate.document/format entry)])

(defmulti build
  "Generate structured (EDN) document content for an entry from a source format. Takes an entry and returns a document (entry)."
  {:malli/schema (:malli/schema (meta #'build-dispatch))}
  build-dispatch)

;; if no build method is implemented for this entry, just pass it through
;; unaltered
(defmethod build :default [entry _opts] entry)

(comment
  {:term/definition
   {:source     (URI. "https://www.merriam-webster.com/dictionary/assemble")
    :definition "to fit together the parts of"}})


(defn assemble
  "Prepare the entries for `produce!` by calling `build` on each entry, then running `tasks` on the results."
  {:term/definition {:source
                     (URI.
                      "https://www.merriam-webster.com/dictionary/assemble")
                     :definition "to fit together the parts of"}
   :malli/schema    site-fn-schema}
  [tasks
   {:keys [site.fabricate.api/entries site.fabricate.api/options] :as site}]
  (let [sort-fn     (get options :site.fabricate.api/entry-sort-fn identity)
        doc-entries (mapv (fn [e] (build e options)) (sort-fn entries))]
    (reduce (fn [site task] (task site))
            (assoc site :site.fabricate.api/entries doc-entries)
            tasks)))

(defn produce-dispatch
  "Return the document and page format for an entry."
  {:malli/schema (m/schema [:=> [:cat entry-schema :map]
                            [:tuple :site.fabricate.document/format
                             :site.fabricate.page/format]])}
  [entry options]
  [(:site.fabricate.document/format entry) (:site.fabricate.page/format entry)])

(defmulti produce!
  "Produce the content of a file from the results of the `build` operation and write it to disk. Takes an entry and returns an entry."
  {:malli/schema (:malli/schema (meta #'produce-dispatch))
   :term/definition
   {:source (URI. "https://www.merriam-webster.com/dictionary/produce")
    :definition
    "to make available for public exhibition or dissemination; to cause to have existence or to happen; to give being, form, or shape to; to compose, create, or bring out by intellectual or physical effort; to bear, make, or yield something"}}
  produce-dispatch)

(defmethod produce! :default [entry _opts] entry)


(defn construct!
  "Run the tasks necessary to complete the website. Execute `produce` on every page, then run `tasks`."
  {:malli/schema site-fn-schema}
  [tasks
   {:keys [site.fabricate.api/entries site.fabricate.api/options]
    :as   init-site}]
  (let [sorted-tasks   tasks
        sorted-entries entries]
    (doseq [e entries] (produce! e options))
    (reduce (fn [site task] (task site)) init-site sorted-tasks)))


(comment)
