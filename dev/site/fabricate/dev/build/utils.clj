(ns site.fabricate.dev.build.utils
  "Page-building utility functions"
  (:require [site.fabricate.adorn :refer [clj->hiccup]]
            [zprint.core :as zp]))

(defn expr->hiccup
  [form]
  (clj->hiccup (zp/zprint-str form
                              {:width 60
                               :style [:community :justified]
                               :map   {:comma? false :sort? false}})))
