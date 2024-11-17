(ns site.fabricate.dev.build.utils
  "Page-building utility functions"
  (:require [site.fabricate.adorn :refer [clj->hiccup]]
            [rewrite-clj.node :as node]
            [zprint.core :as zp]))

(def zprint-defaults
  {:width 60 :style [:community :justified] :map {:comma? false :sort? false}})

(defn expr->hiccup
  ([form opts]
   (let [frm form]
     (clj->hiccup (zp/zprint-str frm (merge zprint-defaults opts)))))
  ([form] (expr->hiccup form {})))

(defn str->hiccup
  ([form-str opts] (expr->hiccup form-str (assoc opts :parse-string-all? true)))
  ([form-str] (str->hiccup form-str {})))
