(ns site.fabricate.prototype.page
  "Namespace providing defaults for output HTML pages"
  (:require [site.fabricate.api :as api]
            [site.fabricate.prototype.eval :as eval]
            [site.fabricate.adorn :as adorn]
            [site.fabricate.prototype.page.hiccup :as hiccup]
            [hiccup.page]
            [clojure.walk :as walk]
            [malli.core :as m]))


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
  [form]
  [:pre {:class "clojure-block" :data-kind (str (name (:kind form)))}
   [:code {:class "language-clojure"} (adorn/clj->hiccup (:value form))]])

(defmethod api/display-form [:hiccup :hiccup/html] [form] (:value form))

(defmethod api/display-form [:edn :hiccup/html]
  [form]
  [:pre {:class "clojure-block" :data-kind (str (name (:kind form)))}
   [:code {:class "language-clojure"} (adorn/clj->hiccup (:value form))]])


(def kindly-map?
  "Returns true if the value is an evaluated Kindly map"
  (m/validator eval/Evaluated-Form))

(defn process-kinds
  "Walk the given form and use api/render-form to produce output values for all embedded Kindly maps"
  [form page-format]
  (walk/postwalk (fn process? [v] (if (kindly-map? v) (api/render-form v) v))
                 form))

(defn render-hiccup-article
  "Return an entry with rendered Hiccup + HTML data suitable for output."
  [{doc-data :site.fabricate.document/data
    page-fmt :site.fabricate.page/format
    :as      entry} opts]
  (let [processed-hiccup (list (hiccup/entry->hiccup-head entry opts)
                               [:body
                                [:main (process-kinds doc-data page-fmt)]])
        output-html      (hiccup.page/html5 processed-hiccup)]
    (assoc entry
           :site.fabricate.page/hiccup processed-hiccup
           :site.fabricate.page/html   output-html)))


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
