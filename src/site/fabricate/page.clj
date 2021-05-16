(ns site.fabricate.page
  "Utility functions for working with HTML page elements."
  (:require [hiccup2.core :as hiccup]
            [hiccup.page :as hp]
            [hiccup.util :as hu]
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
