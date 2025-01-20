(ns site.fabricate.prototype.source.fabricate
  "Default data processing functions for Fabricate's page templates."
  (:require [site.fabricate.prototype.read :as read]
            [site.fabricate.prototype.read.grammar :as grammar]
            [site.fabricate.prototype.hiccup :as hiccup]
            [site.fabricate.api :as api]))

(defn fabricate-v0->hiccup
  "Generate a Hiccup representation of the page by evaluating the parsed Fabricate template of the page contents."
  [entry opts]
  (let [parsed-page    (read/parse (slurp (:site.fabricate.source/location
                                           entry)))
        evaluated-page (read/eval-all parsed-page)
        page-metadata  (hiccup/lift-metadata evaluated-page
                                             (let [m (:metadata
                                                      (meta evaluated-page))]
                                               ;; TODO: better handling of
                                               ;; unbound metadata vars
                                               (if (map? m) m {})))
        hiccup-page    [:html
                        (hiccup/doc-header
                         ;; an example of passing in site-wide config via
                         ;; the options - need to define a more
                         ;; comprehensive list of metadata keys
                         (merge (select-keys opts [:site-title :site-name])
                                page-metadata))
                        [:body
                         [:main
                          (apply conj
                                 [:article {:lang "en-us"}]
                                 (hiccup/parse-paragraphs evaluated-page))]
                         [:footer [:div [:a {:href "/"} "Home"]]]]]]
    (assoc entry
           :site.fabricate.document/data hiccup-page
           :site.fabricate.page/title    (:title page-metadata))))

(defmethod api/build [::grammar/template-v0 :hiccup]
  ([entry opts] (fabricate-v0->hiccup entry opts)))
