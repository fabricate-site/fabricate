(ns site.fabricate.prototype.page.hiccup
  "Default API methods for converting Hiccup documents into pages."
  (:require [hiccup2.core :as hiccup]
            [hiccup.page :as hiccup-page]
            [clojure.java.io :as io]))

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
  ([{hiccup-data :site.fabricate.document/data :as entry} opts]
   [:body [:main hiccup-data]])
  ([entry] (entry->hiccup-body entry {})))

(defn entry->hiccup-head
  "Generate a HTML <head> Hiccup element from the entry."
  ([{doc-title :site.fabricate.document/title :as entry} opts]
   [:head [:title doc-title] [:meta {:charset "utf-8"}]
    [:meta {:http-equiv "X-UA-Compatible" :content "IE-edge"}]
    [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
    [:style css-reset]])
  ([entry] (entry->hiccup-head entry {})))


(defn hiccup-entry->html-entry
  "Generate an entry with a HTML5 string from the given Hiccup entry.

Pass a function as the `:entry->head` key in the options map to generate a header from the contents of the entry. Pass a function as the `:entry->body` key to generate the body from the entry (including the data)"
  [{hiccup-data :site.fabricate.document/data :as entry}
   {:keys [entry->body entry->head]
    :or   {entry->head entry->hiccup-head entry->body entry->hiccup-body}
    :as   opts}]
  (let [html (hiccup-page/html5 [:html (entry->head entry opts)
                                 (entry->body entry opts)])]
    (assoc entry :site.fabricate.page/data html)))


(comment
  (hiccup-page/html5 [:div "something"])
  (hiccup-page/html5 [:html [:head nil] [:body [:div "something"]]]))
