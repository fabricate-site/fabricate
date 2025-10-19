(ns site.fabricate.prototype.template
  "Generate documents from Fabricate source templates"
  (:require [site.fabricate.prototype.read :as read]
            [site.fabricate.prototype.read.grammar :as grammar]
            [site.fabricate.prototype.hiccup :as hiccup]))


(defn entry->hiccup-article
  "Return a Hiccup article for the document by parsing and evaluating the document's Fabricate template."
  [entry opts]
  (let [parsed-page    (read/parse (slurp (:site.fabricate.source/location
                                           entry)))
        evaluated-page (read/eval-all parsed-page)
        page-metadata  (hiccup/lift-metadata evaluated-page
                                             (let [m (:metadata
                                                      (meta evaluated-page))]
                                               ;; TODO: better handling of
                                               ;; unbound metadata vars
                                               (if (map? m) m {})))]
    (with-meta (into [:article
                      {:lang  "en-us"
                       :title (:site.fabricate.page/title page-metadata)}]
                     (hiccup/parse-paragraphs evaluated-page))
               page-metadata)))
