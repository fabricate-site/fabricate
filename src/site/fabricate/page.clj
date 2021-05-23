(ns site.fabricate.page
  "Utility functions for working with HTML page elements."
  (:require
   [site.fabricate.html :as html]
   [hiccup2.core :as hiccup]
   [hiccup.page :as hp]
   [hiccup.util :as hu]
   [clojure.data.finger-tree :as ftree :refer
    [counted-double-list ft-split-at ft-concat]]
   [malli.core :as m]
   [clojure.string :as string]))

(defn doc-header
  "Returns a default header from a map with a post's metadata."
  [{:keys [title page-style scripts]}]
  (let [page-header
        (apply conj
               [:head
                [:title (str "My website | " title)]
                [:meta {:charset "utf-8"}]
                [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge,chrome=1"}]
                [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0, user-scalable=no"}]
                [:script {:type "text/javascript" :src "js/prism.js" :async "async"}]
                [:script {:type "text/javascript" :src "https://cdnjs.cloudflare.com/ajax/libs/prism/1.17.1/plugins/autoloader/prism-autoloader.min.js"}]
                [:link {:type "text/css" :href "css/fonts.css" :rel "stylesheet"}]
                [:link {:type "text/css" :href "css/main.css" :rel "stylesheet"}]]
               scripts)]
    (if page-style (conj page-header [:style page-style]) page-header)))

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
  "For each string in the element split it by the given regex, and insert the result into the original element. Leaves sub-elements as is and inserts them into the preceding paragraph."
  ([seq re]
   (let [v? (vector? seq)
         r (loop [s (apply ftree/counted-double-list seq)
                  final (ftree/counted-double-list)]
             (if (empty? s) final       ; base case
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

(comment
  (detect-paragraphs [:section "some\n\ntext" [:em "with emphasis"]]
                     #"\n\n")

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
             (detect-paragraphs (process-nexts content) #"\n\n")))
    :else
    (let [[content] chunk]
      (apply conj
             [:section]
             (detect-paragraphs (process-nexts content) #"\n\n")))))

(def sectionize-contents
  (comp
   (partition-by section?)
   (partition-all 2)
   (map process-chunk)))

(defn doc-header
  "Returns a default header from a map with a post's metadata."
  [{:keys [title page-style scripts site-title]
    :or {site-title "Fabricate"}}]
  (let [page-header
        (apply conj
               [:head
                [:title (str "Fabricate | " title)]
                [:meta {:charset "utf-8"}]
                [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge,chrome=1"}]
                [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0, user-scalable=no"}]]
               scripts)]
    (if page-style (conj page-header [:style page-style]) page-header)))
