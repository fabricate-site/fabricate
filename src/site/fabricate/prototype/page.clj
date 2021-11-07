(ns site.fabricate.prototype.page
  "Utility functions for working with Hiccup elements."
  (:require
   [site.fabricate.prototype.html :as html]
   [hiccup2.core :as hiccup]
   [hiccup.page :as hp]
   [hiccup.util :as hu]
   [clojure.data.finger-tree :as ftree :refer
    [counted-double-list ft-split-at ft-concat]]
   [malli.core :as m]
   [clojure.string :as string]
   [site.fabricate.prototype.read :as read]
   [site.fabricate.prototype.schema :as schema]))

(defn em [& contents]  (apply conj [:em] contents))
(defn strong [& contents]  (apply conj [:strong] contents))

(defn link
  ([url
    {:keys [frag]
     :or {frag nil}
     :as opts} & contents]
   (let [c (if (not (map? opts)) (conj contents opts) contents)]
     (apply conj [:a {:href url}] c))))

(defn code ([& contents] [:pre (apply conj [:code] contents)]))
(defn in-code ([& contents] (apply conj [:code] contents)))
(defn aside [& contents] (apply conj [:aside] contents))

(defn blockquote
  [{:keys [caption url author source]
    :or {caption nil
         author ""
         url ""}
    :as opts} & contents]
  (let [c (if (not (map? opts)) (conj contents opts) contents)
        s (if source [:figcaption author ", " [:cite source]]
              [:figcaption author])]
    [:figure
     (apply conj [:blockquote {:cite url}] c) s]))

(defn quote [{:keys [cite]
              :or {cite ""}
              :as opts} & contents]
  (let [c (if (not (map? opts)) (conj contents opts) contents)]
    (apply conj [:q {:cite cite}] c)))

(defn ul [& contents]
  (apply conj [:ul] (map (fn [i] [:li i]) contents)))
(defn ol [& contents]
  (apply conj [:ol] (map (fn [i] [:li i]) contents)))

(defn script [attr-map & contents]
  (apply conj [:script attr-map] contents))

(defn not-in-form? [e] (and (vector? e)
                            (not (contains? html/phrasing-tags (first e)))))

(defn para? [i] (and (vector? i) (= :p (first i))))
(defn in-para? [i]
  (or (m/validate html/atomic-element i)
      (and (vector? i) (html/phrasing-tags (first i)))))

(defn detect-paragraphs
  {:doc "For each string in the element split it by the given regex, and insert the result into the original element. Leaves sub-elements as is and inserts them into the preceding paragraph."
   :deprecated true
   :malli/schema [:=> [:cat [:? [:fn seq?]]
                        [:? schema/regex]]
                  html/element]}
  ([seq re]
   (let [v? (vector? seq)
         r (loop [s (apply ftree/counted-double-list seq)
                  final (ftree/counted-double-list)]
             (cond
                                        ; skip paragraph detection on phrasing elements
               (or (html/phrasing? seq)
                   (html/heading? seq)) seq
                                        ; terminating case
               (empty? s) final
               :else
               (let [h (first s) t (rest s)
                     current-elem (last final)]
                 (cond
                   (and (string? h)
                        (some? (re-find re h)))
                   (let [[hh & tt] (string/split h re)
                         rest (map (fn [i] [:p i]) tt)]
                     (cond
                       (or (empty? hh) (re-matches (re-pattern "\\s+") hh)) (recur (concat rest t) final)
                       ((get html/element-validators ::html/p) current-elem)
                       (recur (concat rest t)
                              (conj (first (ft-split-at final (- (count final) 1)))
                                    (conj current-elem hh)))
                       :else (recur (concat rest t) (conj final [:p hh]))))
                   ;; non-string atomic elements remain in the current element
                   (and (not (string? h)) (html/atomic-element? h))
                   (recur t
                          (conj (first (ft-split-at final (- (count final) 1)))
                                (conj current-elem h)))
                   (html/phrasing? h)
                   (if ((get html/element-validators ::html/p) current-elem)
                     (recur t
                            (conj (first (ft-split-at final (- (count final) 1)))
                                  (conj current-elem h)))
                     (recur t (conj final [:p h])))
                   :else
                   (recur t (conj final h))))))]
     (if v? (apply vector r)  r)))
  ([re] (fn [seq] (detect-paragraphs seq re)))
  ([] (detect-paragraphs (re-pattern "\n\n"))))

(defn split-paragraphs
  ([s re]
   (if (and (string? s) (re-find #"\n\n" s))
     (clojure.string/split s #"\n\n")
     s)))

(defn- non-hiccup-seq? [form]
  (and (seq? form)
       (not (string? form))
       (or (not (vector? form))
           (not (keyword? (first form))))))

(defn parse-paragraphs
  "Detects the paragraphs within the form"
  ([form {:keys [paragraph-pattern
                 default-form
                 current-paragraph?]
          :or {paragraph-pattern #"\n\n"
               default-form [:p]
               current-paragraph? false}
          :as opts}]
   (cond  ;; (html/phrasing? form) form
         (non-hiccup-seq? form)
         ;; recurse?
         (parse-paragraphs (apply conj default-form form) opts)
         :else
         (reduce
          (fn [acc next]
            ;; peek the contents of acc to determine whether to
            ;; conj on to an extant paragraph
            (let [previous (peek acc)
                  r-acc (if (not (empty? acc)) (pop acc) acc)
                  previous-paragraph?
                  (and (vector? previous) (= :p (first previous)))
                  current-paragraph? (or current-paragraph? (= :p (first acc)))
                  permitted-contents
                  (if (html/phrasing? acc) ::html/phrasing-content
                      (html/permitted-contents
                       (let [f (first acc)]
                         (if (keyword? f) f :div))))]
              (cond
                ;; flow + heading content needs to break out of a paragraph
                (and current-paragraph? (sequential? next)
                     (or (html/flow? next) (html/heading? next))
                     (not (html/phrasing? next)))
                [acc (parse-paragraphs next opts)]
                ;; if previous element is a paragraph,
                ;; conj phrasing elements on to it
                (and (sequential? next) previous-paragraph?
                     (html/phrasing? next))
                (conj r-acc (conj previous (parse-paragraphs next opts)))
                ;; in-paragraph linebreaks are special, they get replaced with <br> elements
                ;; we can't split text into paragraphs inside
                ;; elements that can't contain paragraphs
                (and (string? next) (re-find paragraph-pattern next)
                     (not (= ::html/flow-content permitted-contents)))
                (apply conj acc
                       (let [r (interpose [:br] (clojure.string/split next paragraph-pattern))]
                         (if (= 1 (count r)) (conj (into [] r) [:br]) r)))
                ;; if there's a previous paragraph, do a head/tail split of the string
                (and (string? next) (re-find paragraph-pattern next)
                     previous-paragraph?)
                (let [[h & tail] (clojure.string/split next paragraph-pattern)]
                  (apply conj
                         r-acc
                         (conj previous h)
                         (->> tail
                              (filter #(not (empty? %)))
                              (map #(conj default-form %)))))
                ;; otherwise split it into separate paragraphs
                (and (string? next) (re-find paragraph-pattern next))
                (apply conj acc
                       (->> (clojure.string/split next paragraph-pattern)
                            (filter #(not (empty? %)))
                            (map #(conj default-form %))))
                ;; skip empty or whitespace strings
                (and (string? next)
                     (or (empty? next)
                         (re-matches #"\s+" next))) acc
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
                (conj acc (parse-paragraphs
                           next
                           (assoc opts :current-paragraph? current-paragraph?)))
                :else (conj acc next))))
          []
          form)))
  ([form] (parse-paragraphs form {})))

(comment
  (non-hiccup-seq? [:p "some text\n\nwith newlines"])

    (parse-paragraphs [:section "some text\n\nwith newlines"])


  (parse-paragraphs [:p "some text\n\nwith newlines"])

  (split-paragraphs "some text\n\nwith linebreak")

  )

(comment
  


  (parse-paragraphs
   [:b [:dfn [:del "some\n\n"] "text"]])

  (parse-paragraphs
   [:b [:dfn "some\n\n"]])

  (html/element? (parse-paragraphs [:del "some\n\n"]))

  ()
  )

(defn process-nexts [nexts]
  (loop [[h n & rest] nexts
         res []]
    (if (empty? rest) ; base case
      (condp = [(nil? h) (nil? n)]
        [true true] res
        [false true] (conj res h)
        [false false] (conj res h n))
      (cond
        (= :next n)
        (recur (apply conj [:next] rest)
               (conj res h))
        (= :next h)
        (recur
         (drop-while #(not= % :next) rest)
         (conj res (apply conj n (take-while #(not= % :next) rest))))
        :else
        (recur
         (drop-while #(not= % :next) rest)
         (apply conj res h n (take-while #(not= % :next) rest)))))))

(defn front-matter? [_] false)

(defn section? [i]
  (and (vector? i) (= :section (first i))))

(defn process-chunk [chunk]
  (cond
    (front-matter? chunk) chunk         ; front matter gets left as-is
    (section? (first (first chunk)))
    (let [[[s] content] chunk]
      (apply conj s
             (parse-paragraphs (process-nexts content))))
    :else
    (let [[content] chunk]
      (apply conj
             [:section]
             (parse-paragraphs (process-nexts content))))))

(def sectionize-contents
  (comp
   (partition-by section?)
   (partition-all 2)
   (map process-chunk)))

(defn ->meta [[k v]]
  (let [attrs (if (map? v) v {:content v})]
    [:meta (merge {:name k} attrs)]))

(defn metadata-map->head-elements
  "Return the contents of the metadata map as a sequence of Hiccup elements"
  [{:keys [page-style scripts title]          ; some keys are special
    :as metadata}]
  (let [rest (dissoc metadata :page-style :scripts)]
    []
    (apply read/conj-non-nil
           (map ->meta rest)
           page-style
           scripts)))

(defn opengraph-enhance
  "Enriches the metadata items given by mapping from metadata names to opengraph properties.

  See https://stackoverflow.com/a/22984013 for more context on combining these attributes in
  a single HTML <meta> element."
  [prop-names items]
  (map (fn [[t attr]]
         (let [meta-name (:name attr)]
           (if (and (= :meta t) (prop-names meta-name))
             [t (assoc attr :property (prop-names meta-name))]
             [t attr])))
       items))

(def default-metadata-map
  {:title "Fabricate"
   :description "Fabricate: static website generation for Clojure"
   "viewport" "width=device-width, initial-scale=1.0"
   :locale "en_US"
   :site-name "fabricate.site"
   :site-title "Fabricate"}
  )

(def default-metadata
  (list [:meta {:charset "utf-8"}]
        [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]))


(def opengraph-property-map
  {:title "og:title"
   :description "og:description"
   :locale "og:locale"
   :site-name "og:site_name"})

(comment
  (map ->meta default-metadata)

  (opengraph-enhance {"description" "og:description"}
                     [[:meta {:name "description" :content "demo desc"}]
                      [:meta {:name "title" :content "demo"}]])

  (opengraph-enhance {"description" "og:description"}
                     (map ->meta default-metadata))

  )

(def ogp-properties
  {:title "og:title"
   :description "og:description"
   :site-title "og:site_name"})

(defn doc-header
  "Returns a default header from a map with a post's metadata."
  [{:keys [title page-style scripts]
    :as metadata}]
  (let [page-meta
        (-> metadata
            (dissoc :title :page-style :scripts)
            (#(merge default-metadata-map %)))]
    (apply read/conj-non-nil
           [:head
            [:title (str (:site-title page-meta) " | " title)]
            [:link {:rel "stylesheet" :href "https://raw.githubusercontent.com/jensimmons/cssremedy/master/css/remedy.css"}]
            [:link {:rel "stylesheet" :href "css/extras.css"}]]
           (concat (opengraph-enhance
                    ogp-properties
                    (map ->meta page-meta))
                   default-metadata
                   (if scripts scripts)
                   (if page-style [[:style page-style]])))))
