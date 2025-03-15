(ns site.fabricate.document
  (:require [site.fabricate.api :as api]
            [site.fabricate.prototype.document.clojure :as clj]
            [site.fabricate.prototype.document.fabricate :as fabricate]))


(def defaults "Default options for documents" {})

(defmethod api/build [::fabricate/v0 :hiccup/article]
  [entry opts]
  (let [article          (fabricate/entry->hiccup-article entry opts)
        article-attrs    (nth article 1)
        article-metadata (meta article)]
    (merge entry
           {:site.fabricate.document/data     article
            :site.fabricate.page/title        (:title article-attrs)
            :site.fabricate.document/metadata article-metadata})))

(defmethod api/build [:clojure/file :hiccup/article]
  [{:keys [site.fabricate.source/location] :as entry} opts]
  (let [article          (-> location
                             clj/read-forms
                             clj/eval-forms
                             clj/forms->hiccup)
        article-attrs    (nth article 1)
        article-metadata (meta article)]
    (merge entry
           {:site.fabricate.document/title    (:title
                                               article-attrs
                                               (:site.fabricate.document/title
                                                article-metadata))
            :site.fabricate.document/metadata article-metadata
            :site.fabricate.document/data     article})))

(defmethod api/build [:clojure/file :kind/fragment]
  [{:keys [site.fabricate.source/location] :as entry} opts]
  (let [fragment (-> location
                     clj/read-forms
                     clj/eval-forms
                     clj/forms->fragment)
        ns-meta  (meta (find-ns (:clojure/namespace (meta fragment))))]
    (merge entry
           {:site.fabricate.document/metadata (merge ns-meta (meta fragment))
            :site.fabricate.document/data     fragment})))

(defmethod api/build [:clojure/file ::clj/forms]
  [{:keys [site.fabricate.source/location] :as entry} opts]
  (let [forms    (-> location
                     clj/read-forms
                     clj/eval-forms)
        forms-ns (:clojure/namespace forms)]
    (merge entry
           {:site.fabricate.document/data forms
            :clojure/namespace forms-ns
            :site.fabricate.document/metadata (meta (find-ns forms-ns))})))
