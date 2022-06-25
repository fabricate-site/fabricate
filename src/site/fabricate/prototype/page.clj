(ns site.fabricate.prototype.page
  "Functions for transforming processed page contents."
  (:require
   [site.fabricate.prototype.html :as html]
   [hiccup2.core :as hiccup]
   [hiccup.page :as hp]
   [hiccup.util :as hu]
   [clojure.data.finger-tree :as ftree :refer
    [counted-double-list ft-split-at ft-concat]]
   [malli.core :as m]
   [clojure.string :as string]
   [clojure.repl :refer [source-fn]]
   [rewrite-clj.node :as node :refer [tag sexpr]]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]
   [site.fabricate.prototype.read :as read]
   [site.fabricate.prototype.read.grammar :as grammar]
   [site.fabricate.prototype.schema :as schema]))

(defn em
  {:malli/schema [:=> [:cat [:* :any]] [:cat [:= :em] [:* :any]]]}
  [& contents]  (apply conj [:em] contents))

(defn strong
  {:malli/schema [:=> [:cat [:* :any]] [:cat [:= :em] [:* :any]]]}
  [& contents]  (apply conj [:strong] contents))

(defn link
  {:malli/schema [:=> [:cat :string [:? :map] [:* :any]]
                  [:cat [:= :a] [:map [:href :string]] [:* :any]]]}
  ([url
    {:keys [frag]
     :or {frag nil}
     :as opts} & contents]
   (let [c (if (not (map? opts)) (conj contents opts) contents)]
     (apply conj [:a {:href url}] c))))

(defn code
  {:malli/schema
   [:=> [:cat [:* :any]]
    [:cat [:= :pre] [:schema [:cat [:= :code] [:* :any]]]]]}
  [& contents]
  [:pre (apply conj [:code] contents)])

(defn in-code
  {:malli/schema [:=> [:cat [:* :any]]
                  [:cat [:= :code] [:* :any]]]}
  [& contents] (apply conj [:code] contents))

(defn aside {:malli/schema [:=> [:cat [:* :any]]
                            [:cat [:= :aside] [:* :any]]]}
  [& contents] (apply conj [:aside] contents))

(defn blockquote
  {:malli/schema
   [:=> [:cat [:? :map] [:* :any]]
    [:cat [:= :figure]
     [:schema [:cat [:= :blockquote]
               :map
               [:* :any]
               [:? [:schema [:cat [:= :figcaption] [:* :any]]]]]]]]}
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

(defn quote
  {:malli/schema
   [:=> [:cat [:? :map] [:* :any]]
    [:cat [:= :q] :map [:* :any]]]}
  [{:keys [cite]
    :or {cite ""}
    :as opts} & contents]
  (let [c (if (not (map? opts)) (conj contents opts) contents)]
    (apply conj [:q {:cite cite}] c)))

(defn ul
  {:malli/schema
   [:=> [:cat [:* :any]]
    [:cat [:= :ul] [:* [:schema [:cat [:= :li] :any]]]]]}
  [& contents]
  (apply conj [:ul] (map (fn [i] [:li i]) contents)))

(defn ol
  {:malli/schema
   [:=> [:cat [:* :any]]
    [:cat [:= :ol] [:* [:schema [:cat [:= :li] :any]]]]]}
  [& contents]
  (apply conj [:ol] (map (fn [i] [:li i]) contents)))

(defn script
  {:malli/schema
   [:=> [:cat :map [:* :any]]
    [:cat [:= :script] [:? :map] [:* :any]]]}
  [attr-map & contents]
  (apply conj [:script attr-map] contents))

(defn not-in-form?
  {:malli/schema [:=> [:cat :any] :boolean]}
  [e]
  (and (vector? e)
       (not (contains? html/phrasing-tags (first e)))))

(defn para?
  {:malli/schema [:=> [:cat :any] :boolean]}
  [i]
  (and (vector? i) (= :p (first i))))
(defn in-para?
  {:malli/schema [:=> [:cat :any] :boolean]}
  [i]
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
  {:malli/schema
   [:=> [:cat :string [:fn #(= java.util.regex.Pattern (type %))]]
    [:or [:* :string] :string]]}
  ([s re]
   (if (and (string? s) (re-find #"\n\n" s))
     (clojure.string/split s #"\n\n")
     s)))

(defn- non-hiccup-seq? [form]
  (and (seq? form)
       (not (string? form))
       (or (not (vector? form))
           (not (keyword? (first form))))))

(defn- reconstruct [s sequence-type]
  (cond (= clojure.lang.PersistentList sequence-type)
        (apply list s)
        (= clojure.lang.PersistentVector sequence-type)
        (into [] s) :else (into [] s)))

(defn- final-match? [re s]
  (and (= 1 (count (re-seq re s)))
       (let [m (re-matcher re s)]
         (.find m)
         (= (.end m) (count s)))))

(comment

  (re-seq #"\n\n" "abc\n\ndef")

  (final-match? #"\n\n" "abc\n\n")

  (final-match? #"\n\n" "abc")

  (iterator-seq (.iterator (.results (re-matcher #"\n\n" "abc\n\ndef\n\nghi"))))

  (loop [ct 0 m (re-matcher #"\n\n" "abc\n\ndef\n\nghi")]
    (let [f? (.find m)]
      (if (not f?) ct
          (recur (inc ct) m))))

  (loop [ct 0 m (re-matcher #"\n\n" "abc\n\ndef\n\nghi")]
    (let [f? (.find m)]
      (if (not f?) ct
          (recur (inc ct) m)))))

(defn parse-paragraphs
  "Detects the paragraphs within the form"
  {:malli/schema [:=> [:cat [:vector :any] :map] [:vector :any]]}
  ([form {:keys [paragraph-pattern
                 default-form
                 current-paragraph?]
          :or {paragraph-pattern #"\n\n"
               default-form [:p]
               current-paragraph? false}
          :as opts}]
   (let [sequence-type (type form)
         res
         (cond
           ; don't detect in specific elements
           (#{:svg :dl :figure :pre} (first form)) form
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
                    previous-paragraph?
                    (and (vector? previous) (= :p (first previous)))
                    current-paragraph? (or current-paragraph? (= :p (first acc)))
                    permitted-contents
                    (if (html/phrasing? acc) ::html/phrasing-content
                        (html/tag-contents
                         (let [f (first acc)]
                           (if (keyword? f) f :div))))]
                (cond
                  ;; flow + heading content needs to break out of a paragraph
                  (and current-paragraph? (sequential? next)
                       (or (html/flow? next) (html/heading? next))
                       (not (html/phrasing? next)))
                  (counted-double-list
                   (reconstruct acc sequence-type)
                   (parse-paragraphs next opts))
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
                  ;; corner case: trailing paragraph-pattern
                  (and (string? next) (re-find paragraph-pattern next)
                       (not previous-paragraph?)
                       (not current-paragraph?)
                       (final-match? paragraph-pattern next))
                  (conj acc [:p (first (string/split next paragraph-pattern)) [:br]])
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
                  ;; or not:
                  #_ #_
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
            (counted-double-list)
            form))]
     (reconstruct res sequence-type)))
  ([form] (parse-paragraphs form {})))

(comment
  (non-hiccup-seq? [:p "some text\n\nwith newlines"])

  (parse-paragraphs [:section "some text\n\nwith newlines"])

  (parse-paragraphs [:p "some text\n\nwith newlines"])

  (split-paragraphs "some text\n\nwith linebreak")

  (parse-paragraphs
   [:b [:dfn [:del "some\n\n"] "text"]])

  (parse-paragraphs
   [:b [:dfn "some\n\n"]])

  (html/element? (parse-paragraphs [:del "some\n\n"])))

(defn ->meta
  {:malli/schema
   [:=> [:cat [:schema [:cat :any :any]]]
    [:cat [:= :meta] :map]]}
  [[k v]]
  (let [attrs (if (map? v) v {:content v})]
    [:meta (merge {:name (if (keyword? k) (str (name k)) k)} attrs)]))

(defn metadata-map->head-elements
  "Return the contents of the metadata map as a sequence of Hiccup elements"
  {:malli/schema [:=> [:cat :map] [:vector :any]]}
  [{:keys [page-style scripts title]    ; some keys are special
    :as metadata}]
  (let [rest (dissoc metadata :page-style :scripts)]
    (apply read/conj-non-nil
           (map ->meta rest)
           page-style
           scripts)))

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

(def default-metadata-map
  {:title "Fabricate"
   :description "Fabricate: static website generation for Clojure"
   "viewport" "width=device-width, initial-scale=1.0"
   :locale "en_US"
   :site-name "fabricate.site"
   :site-title "Fabricate"})

(def default-metadata
  (list [:meta {:charset "utf-8"}]
        [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]))

(def opengraph-property-map
  {:title "og:title"
   "title" "og:title"
   :description "og:description"
   "description" "og:description"
   :locale "og:locale"
   "locale" "og:locale"
   :site-name "og:site_name"
   "site-name" "og:site_name"})

(comment
  (map ->meta default-metadata)

  (opengraph-enhance {"description" "og:description"}
                     [[:meta {:name "description" :content "demo desc"}]
                      [:meta {:name "title" :content "demo"}]])

  (opengraph-enhance {"description" "og:description"}
                     (map ->meta default-metadata)))

(def ogp-properties
  {:title "og:title"
   :description "og:description"
   :site-title "og:site_name"})

(defn doc-header
  "Returns a default header from a map with a post's metadata."
  {:malli/schema [:=> [:cat :map] [:vector :any]]}
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
            [:link {:rel "stylesheet" :href "/css/extras.css"}]]
           (concat (opengraph-enhance
                    ogp-properties
                    (map ->meta page-meta))
                   default-metadata
                   (if scripts scripts)
                   (if page-style [[:style page-style]])))))

(defn- rename-meta [m]
  (reduce
   (fn [m [k v]]
     (if (and (keyword? k) (= "page" (namespace k))) (assoc m (keyword (name k)) v)
         m))
   {}
   m))

(defn lift-metadata
  "Lifts the metadata out of the page contents and into the given metadata map."
  {:malli/schema [:=> [:cat [:vector :any] :map] :map]}
  [page-contents metadata]
  (reduce (fn [m v] (if (meta v) (merge m (rename-meta (meta v)))
                        m))
          metadata
          (tree-seq sequential? identity page-contents)))

;; stop writing dead code: every instance of clojure code embedded in a string in a fabricate document is an antipattern / code smell
;; code is data, it should be handled as such by Fabricate

(defn simple-expr
  "Takes a Clojure form and yields a string with the Fabricate template expression for that form."
  {:malli/schema [:=> [:cat :any :map] :string]}
  [form {:keys [ctrl-char format-fn]
         :or {format-fn str
              ctrl-char ""}}]
  (let [[start end] grammar/delimiters]
    (str start ctrl-char (format-fn form) end)))

(defn- span [class & contents]
  (apply conj
         [:span {:class (str "language-clojure " class)}]
         contents))

(defmulti node->hiccup tag)

(defn- atom-class [node]
  (cond (:k node) "keyword"
        (:lines node) "string"
        (contains? node :value)
        (let [v (:value node)]
          (cond (number? v) "number"
                (string? v) "string"
                (nil? v) "nil"
                (symbol? v) "symbol"
                :else (name (tag node))))
        :else (println (tag node) (keys node)
                       (sexpr node) (type (sexpr node)))))

(defmethod node->hiccup :token [node]
  (span (atom-class node) (hu/escape-html (str node))))

(defmethod node->hiccup :whitespace [node]
  (:whitespace node))

(defmethod node->hiccup :multi-line [node]
  (span "string"
        (interpose [:br] (:lines node))))

(defmethod node->hiccup :map [node]
  (apply span "map" "{" (conj (mapv node->hiccup (:children node)) "}")))

(defn- fn-list? [node]
  (and (= (:tag node) :list) (= 'fn* (:value (first (:children node))))))

(defn- fn-node->hiccup [node]
  (let [contents (:children node)
        [_ params body] (sexpr node)
        r (cond
            (= 0 (count params)) {}
            (= 1 (count params)) {(first params) '%}
            (and (= 2 (count params)) (= '& (first params)))
            {(second params) '%&}
            :else
            (into {} (map-indexed
                      (fn [ix sym]
                        [sym (symbol (str "%" (inc ix)))])
                      params)))
        edited-node
        (z/node (z/postwalk (z/edn (node/coerce body))
                            (fn select [zloc] (symbol? (z/sexpr zloc)))
                            (fn visit [zloc] (z/edit zloc  #(get r % %)))))]
    (apply span (name (tag node))
           "#(" (conj (mapv node->hiccup (:children edited-node)) ")"))))

(defmethod node->hiccup :fn [node]
  ;; this is a really tricky one, as it involves
  ;; rewriting the expanded function to resemble the input
  (fn-node->hiccup node))

(defmethod node->hiccup :list [node]
  (if (fn-list? node) (fn-node->hiccup node)
      (apply span "list" "(" (conj (mapv node->hiccup (:children node)) ")"))))


(defmethod node->hiccup :set [node]
  (apply span (name (tag node))
         "#{" (conj (mapv node->hiccup (:children node)) "}")))

(defmethod node->hiccup :newline [node]
  (repeat (count (:newlines node)) [:br]))

(defmethod node->hiccup :vector [node]
  (apply span "vector" "[" (conj (mapv node->hiccup (:children node)) "]")))

(defmethod node->hiccup :meta [node]
  (apply span (name (tag node)) "^" (mapv node->hiccup (:children node))))

(defmethod node->hiccup :comma [node]
  (span (name (tag node)) ","))

(defmethod node->hiccup :comment [node]
  (span (name (tag node)) (:prefix node)
        (string/replace (:s node) "\n" "")
        [:br]))

(-> "; a comment\n; another comment\n(+ 1 1)"
    p/parse-string-all)


(defn expr->hiccup
  "Converts the given expression into a hiccup element tokenzed into spans by the value type."
  {:malli/schema [:=> [:cat :any] [:vector :any]]}
  [expr]
  (node->hiccup (rewrite-clj.node/coerce expr)))

(defn fn->spec-form
  "Converts the given function symbol into the conformed spec for function definition"
  {:malli/schema [:=> [:cat :symbol] [:vector :any]]}
  [fn-sym]
  (-> fn-sym
      source-fn
      read-string
      rest
      (#(clojure.spec.alpha/conform :clojure.core.specs.alpha/defn-args %))))

(defn str->hiccup
  "Converts the given Clojure string into a hiccup element"
  {:malli/schema [:=> [:cat :string] [:vector :any]]}
  [expr-str]
  (node->hiccup (p/parse-string expr-str)))

(comment
  (clojure.repl/doc p/parse)

  (clojure.repl/doc p/parse-string-all)
  (clojure.repl/doc p/parse-string)

  (node->hiccup (rewrite-clj.node/coerce 'sym))

  (expr->hiccup '(def myvar 183))

  (expr->hiccup '(38 29 "a" :kw sym))
  (expr->hiccup '[38 29 "a" :kw sym])
  (expr->hiccup #{38 29 "a" :kw})
  (expr->hiccup '#(+ 3 %))

  (=
   (rewrite-clj.node/coerce [:em{:class"tiny"}"text"])
   (rewrite-clj.node/coerce [:em {:class "tiny"} "text"])

   )
  (:children
   (rewrite-clj.node/coerce [:em{:class"tiny"}"text"]))

  (:whitespace (second (:children
                        (rewrite-clj.node/coerce [:em {:class "tiny"} "text"]))))

  (expr->hiccup [:em {:class "tiny"} "text"])

  (=
   (rewrite-clj.node/coerce [:em{:class "tiny"}"text"])

   (rewrite-clj.node/coerce [:em {:class "tiny"} "text"]))

  (= (rewrite-clj.node/coerce :a)
     (rewrite-clj.node/coerce :a)
     )



  (rewrite-clj.node/coerce 'sym)

  (fn-node->hiccup (rewrite-clj.node/coerce '#(+ 3 %)))

  (fn-list? (rewrite-clj.node/coerce '#(+ 3 %)))


  (:tag (rewrite-clj.node/coerce '#(+ 3 %)))


  (= 'fn* (:value (first (:children (rewrite-clj.node/coerce `#(+ 3 %))))))


  (let [n (rewrite-clj.node/coerce '#(+ 3 %))]
    (into {} (map (fn [k v] [k v]) (keys n) (vals n))))

  (node->hiccup (rewrite-clj.node/coerce {:a 3})))
