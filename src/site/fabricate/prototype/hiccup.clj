(ns site.fabricate.prototype.hiccup
  "Functions for processing and transforming Hiccup elements and documents."
  (:require [clojure.data.finger-tree :as ftree :refer [counted-double-list]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hiccup.page]
            [site.fabricate.prototype.html :as html]))

(defn- reconstruct
  [s sequence-type]
  (cond (= clojure.lang.PersistentList sequence-type)   (apply list s)
        (= clojure.lang.PersistentVector sequence-type) (into [] s)
        :else                                           (into [] s)))

(defn- final-match?
  [re s]
  (and (= 1 (count (re-seq re s)))
       (let [m (re-matcher re s)]
         (.find m)
         (= (.end m) (count s)))))

(defn parse-paragraphs
  "Detects the paragraphs within the form based on a delimiter and
  separates them into distinct <p> elements."
  {:malli/schema [:=> [:cat [:vector :any] :map] [:vector :any]]}
  ([form
    {:keys [paragraph-pattern default-form current-paragraph?]
     :or   {paragraph-pattern  #"\n\n"
            default-form       [:p]
            current-paragraph? false}
     :as   opts}]
   (let [sequence-type (type form)
         res
         (cond
           ; don't detect in specific elements
           (#{:svg :dl :figure :pre :img :header} (first form)) form
           ;; (non-hiccup-seq? form)
           ;; recurse?
           ;; (parse-paragraphs (apply conj default-form form) opts)
           :else
           (reduce
            (fn [acc next]
              ;; peek the contents of acc to determine whether to
              ;; conj on to an extant paragraph
              (let [previous (peek acc)
                    r-acc (if (not (empty? acc)) (pop acc) acc)
                    previous-paragraph? (and (vector? previous)
                                             (= :p (first previous)))
                    current-paragraph? (or current-paragraph?
                                           (= :p (first acc)))
                    permitted-contents (if (html/phrasing? acc)
                                         ::html/phrasing-content
                                         (html/tag-contents
                                          (let [f (first acc)]
                                            (if (keyword? f) f :div))))]
                (cond
                  ;; flow + heading content needs to break out of a
                  ;; paragraph
                  (and current-paragraph?
                       (sequential? next)
                       (or (html/flow? next) (html/heading? next))
                       (not (html/phrasing? next)))
                  (counted-double-list (reconstruct acc sequence-type)
                                       (parse-paragraphs next opts))
                  ;; if previous element is a paragraph,
                  ;; conj phrasing elements on to it
                  (and (sequential? next)
                       previous-paragraph?
                       (html/phrasing? next))
                  (conj r-acc (conj previous (parse-paragraphs next opts)))
                  ;; in-paragraph linebreaks are special, they get
                  ;; replaced with <br> elements we can't split text
                  ;; into paragraphs inside elements that can't
                  ;; contain paragraphs
                  (and (string? next)
                       (re-find paragraph-pattern next)
                       (not (= ::html/flow-content permitted-contents)))
                  (apply conj
                         acc
                         (let [r (interpose [:br]
                                  (str/split next paragraph-pattern))]
                           (if (= 1 (count r)) (conj (into [] r) [:br]) r)))
                  ;; corner case: trailing paragraph-pattern
                  (and (string? next)
                       (re-find paragraph-pattern next)
                       (not previous-paragraph?)
                       (not current-paragraph?)
                       (final-match? paragraph-pattern next))
                  (conj acc
                        [:p (first (str/split next paragraph-pattern)) [:br]])
                  ;; if there's a previous paragraph, do a head/tail
                  ;; split of the string
                  (and (string? next)
                       (re-find paragraph-pattern next)
                       previous-paragraph?)
                  (let [[h & tail] (str/split next paragraph-pattern)]
                    (apply conj
                           r-acc
                           (conj previous h)
                           (->> tail
                                (filter #(not (empty? %)))
                                (map #(conj default-form %)))))
                  ;; otherwise split it into separate paragraphs
                  (and (string? next) (re-find paragraph-pattern next))
                  (apply conj
                         acc
                         (->> (str/split next paragraph-pattern)
                              (filter #(not (empty? %)))
                              (map #(conj default-form %))))
                  ;; skip empty or whitespace strings or not:
                  #_#_(and (string? next)
                           (or (empty? next) (re-matches #"\s+" next)))
                    acc
                  ;; add to previous paragraph
                  (and previous-paragraph? (html/phrasing? next))
                  (conj r-acc (conj previous next))
                  ;; start paragraph for orphan strings/elements
                  (and (not current-paragraph?)
                       (not previous-paragraph?)
                       (= ::html/flow-content permitted-contents)
                       (html/phrasing? next))
                  (conj acc (conj default-form next))
                  (sequential? next)
                  (conj acc
                        (parse-paragraphs
                         next
                         (assoc opts :current-paragraph? current-paragraph?)))
                  :else (conj acc next))))
            (counted-double-list)
            form))]
     (reconstruct res sequence-type)))
  ([form] (parse-paragraphs form {})))

(defn ->meta
  "Convert the given key/value pair to a Hiccup/HTML metadata element."
  {:malli/schema [:=> [:cat [:schema [:cat :any :any]]] [:cat [:= :meta] :map]]}
  [[k v]]
  (let [attrs (if (map? v) v {:content v})]
    [:meta (merge {:name (if (keyword? k) (str (name k)) k)} attrs)]))


(def default-metadata-map
  "Default metadata for Fabricate HTML pages."
  {:title       "Fabricate"
   :description "Fabricate: static website generation for Clojure"
   "viewport"   "width=device-width, initial-scale=1.0"
   :locale      "en_US"
   :site-name   "fabricate.site"
   :site-title  "Fabricate"})

(def default-metadata
  "Additional default metadata for Fabricate HTML pages."
  (list [:meta {:charset "utf-8"}]
        [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]))



(def opengraph-properties
  "Mapping of strings and keywords to corresponding opengraph properties.

  See https://ogp.me/ for more details."
  {:description  "og:description"
   :locale       "og:locale"
   "locale"      "og:locale"
   "site-name"   "og:site_name"
   :title        "og:title"
   "site-title"  "og:site_name"
   "title"       "og:title"
   :site-name    "og:site_name"
   :site-title   "og:site_name"
   "description" "og:description"})


(defn opengraph-enhance
  "Enriches the metadata items given by mapping from metadata names to opengraph properties.

  See https://stackoverflow.com/a/22984013 for more context on combining these attributes in
  a single HTML <meta> element."
  {:malli/schema [:=> [:cat [:map [:* [:schema [:cat [:= :meta] :map]]]]]
                  [:* [:cat [:= :meta] :map]]]}
  [prop-names items]
  (map (fn [[t attr]]
         (let [meta-name (:name attr)]
           (if (and (= :meta t) (prop-names meta-name))
             [t (assoc attr :property (prop-names meta-name))]
             [t attr])))
       items))


(defn- rename-meta
  [m]
  (reduce (fn [m [k v]]
            (if (and (keyword? k) (= "page" (namespace k)))
              (assoc m (keyword (name k)) v)
              m))
          {}
          m))

(defn lift-metadata
  "Lifts the metadata out of the page contents and into the given metadata map."
  {:malli/schema [:=> [:cat [:vector :any] :map] :map]}
  [page-contents metadata]
  (reduce (fn [m v] (if (meta v) (merge m (rename-meta (meta v))) m))
          metadata
          (tree-seq sequential? identity page-contents)))

;; Fabricate HTML defaults

(def css-reset
  "CSS reset for Fabricate pages"
  (slurp (io/resource "site/fabricate/reset.css")))


(def default-metadata
  "Default metadata to add to page head elements"
  (list [:meta {:charset "utf-8"}]
        [:meta {:http-equiv "X-UA-Compatible" :content "IE-edge"}]
        [:meta
         {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
        [:style css-reset]))

;; defaults
(defn entry->hiccup-body
  "Generate a HTML <body> Hiccup element from the entry."
  {:malli/schema [:-> #'site.fabricate.api/Entry [:vector :any]]}
  ([{hiccup-data :site.fabricate.document/data :as entry} opts]
   [:body [:main hiccup-data]])
  ([entry] (entry->hiccup-body entry {})))

(defn entry->hiccup-head
  "Generate a HTML <head> Hiccup element from the entry."
  {:malli/schema [:-> #'site.fabricate.api/Entry [:vector :any]]}
  ([{doc-title :site.fabricate.document/title :as entry} opts]
   [:head [:title doc-title] [:meta {:charset "utf-8"}]
    [:meta {:http-equiv "X-UA-Compatible" :content "IE-edge"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
    [:style css-reset]])
  ([entry] (entry->hiccup-head entry {})))


(defn hiccup-entry->html-entry
  "Generate an entry with a HTML5 string from the given Hiccup entry.

Pass a function as the `:entry->head` key in the options map to generate a header from the contents of the entry. Pass a function as the `:entry->body` key to generate the body from the entry (including the data)"
  {:malli/schema [:-> #'site.fabricate.api/Entry #'site.fabricate.api/Entry]}
  [{hiccup-data :site.fabricate.document/data :as entry}
   {:keys [entry->body entry->head]
    :or   {entry->head entry->hiccup-head entry->body entry->hiccup-body}
    :as   opts}]
  (let [html (hiccup.page/html5 [:html (entry->head entry opts)
                                 (entry->body entry opts)])]
    (assoc entry :site.fabricate.page/data html)))
