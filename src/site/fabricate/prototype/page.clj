(ns site.fabricate.prototype.page
  (:require [site.fabricate.api :as api]
            [site.fabricate.adorn :as adorn]))

(def display-defaults
  "Default options for each kind"
  {:hiccup  {:hide-value false :hide-code true}
   :code    {:hide-value false :hide-code true}
   :md      {:hide-value false :hide-code true}
   :hidden  {:hide-value true :hide-code true}
   :edn     {:hide-value false :hide-code true}
   ;; nil means default in kindly, but an explicit default is worth
   ;; preserving
   nil      {:hide-value false :hide-code false}
   :default {:hide-value false :hide-code false}})

;; TODO: figure out how to use adorn to render nested kinds
(defmethod api/display-form [:code :hiccup/html]
  ([form _opts]
   [:pre {:class "clojure-block" :data-kind (str (name (:kind form)))}
    [:code {:class "language-clojure"} (adorn/clj->hiccup (:value form))]])
  ([form] (api/display-form form {})))

(defmethod api/display-form [:hiccup :hiccup/html]
  ([form _opts] (:value form))
  ([form] (api/display-form form {})))

(defmethod api/display-form [:edn :hiccup/html]
  ([form _opts]
   [:pre {:class "clojure-block" :data-kind (str (name (:kind form)))}
    [:code {:class "language-clojure"} (adorn/clj->hiccup (:value form))]])
  ([form] (api/display-form form {})))


(comment
  (api/display-form {:value "(+ 3 4)" :kind :code})
  (require '[scicloj.kindly.v4.api :as kindly])
  kindly/known-kinds
  ;; => #{:kind/edn
  ;;      :kind/code
  ;;      :kind/vega
  ;;      :kind/smile-model
  ;;      :kind/image
  ;;      :kind/plotly
  ;;      :kind/echarts
  ;;      :kind/map
  ;;      :kind/portal
  ;;      :kind/test
  ;;      :kind/dataset
  ;;      :kind/vega-lite
  ;;      :kind/html
  ;;      :kind/cytoscape
  ;;      :kind/set
  ;;      :kind/reagent
  ;;      :kind/var
  ;;      :kind/hidden
  ;;      :kind/hiccup
  ;;      :kind/md
  ;;      :kind/tex
  ;;      :kind/seq
  ;;      :kind/htmlwidgets-plotly
  ;;      :kind/video
  ;;      :kind/observable
  ;;      :kind/emmy-viewers
  ;;      :kind/pprint
  ;;      :kind/highcharts
  ;;      :kind/table
  ;;      :kind/fn
  ;;      :kind/vector
  ;;      :kind/htmlwidgets-ggplotly
  ;;      :kind/fragment
  ;;      :kind/scittle
  ;;      :kind/test-last}
)
