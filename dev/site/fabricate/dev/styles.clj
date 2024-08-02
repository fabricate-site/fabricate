(ns site.fabricate.dev.styles
  (:require [garden.core :as garden]
            [garden.selectors :as select]
            [garden.stylesheet :as stylesheet]))

(def ^:private roboto-flex-import-url
  "https://fonts.googleapis.com/css2?family=Roboto+Flex:opsz,wdth,wght@8..144,85..151,100..1000&display=swap")

(def docs
  (list
   (stylesheet/at-import roboto-flex-import-url)
   (stylesheet/at-import
    "https://fonts.googleapis.com/css2?family=Spline+Sans+Mono:wght@300..700&display=swap")
   [:article
    {:font-family         "'Roboto Flex',sans-serif"
     :font-optical-sizing "auto"
     :font-size           "min(4vmin, 20px)"
     :font-variation-settings "'wdth' 91"
     :color               "#222"
     #_#_:margin-left "1vw"
     :line-height         "1.29em"
     :display             "grid"
     :grid-template-columns "repeat(12, minmax(3px, 1fr))"
     :grid-column-gap     "0.5rem"
     :grid-row-gap        "0.5em"}]
   [:aside {:grid-column "5 / 12" :font-size "0.8em" :font-weight "400"}]
   #_[(select/aside select/before)
      {:content "\"📝\"" :left "0" :position "absolute" :grid-column "5"}]
   [:h1 :h2 :h3 :h4 :h5 :h6 :header
    {:font-weight   "900"
     :grid-column   "2 / 10"
     :font-family   "'Roboto Flex',sans-serif"
     :margin-bottom "0.25em"
     :margin-top    "0em"}]
   [:body {:background-color "#E2DED6"}]
   [:footer
    {:font-family "'Roboto Flex', sans-serif"
     :font-variation-settings "'wdth' 125"
     :font-weight "500"}]
   [:p
    {:grid-column     "2 / 10"
     :margin-bottom   "0.5em"
     :padding         "0px"
     :margin-top      "0em"
     :text-align      "justify"
     :text-justify    "inter-word"
     :text-align-last "left"
     :hyphens         "auto"}]
   [:div {:grid-column "2 / 12" :margin-bottom "0em" :margin-top "0em"}]
   [:ul :ol {:grid-column "2 / 11" :margin-top "0em" :margin-bottom "0em"}
    [:li {:margin-bottom "0.4em" :margin-left "-2rem" :padding-left "0em"}]]
   [:figure {:margin-left "0em"}]
   [:figure :blockquote {:grid-column "2 / 10"}]
   [:blockquote {:margin-left "2em" :margin-right "2em"}]
   [:code
    {:font-family      "'Spline Sans Mono', monospace"
     :font-weight      "500"
     :font-size        "0.92em"
     :white-space      "pre-wrap"
     :background-color "#EEE"}]
   [:pre
    {:background-color "#EEE"
     :white-space      "pre-wrap"
     :grid-column      "2 / 12"
     :margin-bottom    "0.5em"
     :font-weight      "450"
     :line-height      "1.45rem"
     :margin-top       "0em"
     :font-family      "'Spline Sans Mono', monospace"}
    [:ins {:text-decoration "none" :background-color "#ADC3A2"}]
    [(select/ins select/before)
     {:position         "relative"
      :background-color "#EEE"
      :font-weight      "700"
      :content          "\"+\""}]
    [(select/ins select/after) {:content "\"\"" :display "block"}]]
   [:dl {:display "grid" :grid-column-gap "0.5em" :grid-row-gap "0.65em"}
    [:dt {:grid-column "1 / 3 !important" :font-weight "600"}]
    [:dd {:grid-column "3 !important" :margin-left "0em" :min-width "25ch"}]]
   [:dl
    [:dl {:display "block"} [:dt {:grid-column "auto"}]
     [:dd {:grid-column "auto"}]]]
   (stylesheet/at-media {:max-width "900px"}
                        [:p :ul :ol {:grid-column "1 / 10"}]
                        [:div {:grid-column "1 / 13"}]
                        [:pre :figure :blockquote {:grid-column "2 / 13"}]
                        [:h1 :h2 :h3 :h4 :h5 :h6 :header
                         {:grid-column "1 / 13"}])
   [:.xl-text
    {:font-family    "Roboto Flex, sans-serif"
     :font-variation-settings "'wdth' 141"
     :font-weight    1000
     :font-size      "2.5em"
     :line-height    "1em"
     :text-transform "uppercase"}]
   [:.l-text
    {:font-family    "Roboto Flex, sans-serif"
     :font-variation-settings "'wdth' 141"
     :font-weight    1000
     :font-size      "1.5em"
     :line-height    "1em"
     :text-transform "uppercase"}]
   [:.language-clojure {:font-family "'Spline Sans Mono', monospace"}
    [:.keyword {:color "#7f2c28"}] [:.string {:color "#7a7533"}]
    [:.symbol {:color "#4f2c28"}] [:.comment {:font-weight "300" :color "#555"}]
    [:.uneval {:font-weight "300" :color "#555"}]]
   [:a {:margin-bottom "0em" :text-decoration "none" :color "#337E80"}]
   [:.repl
    [(select/code select/before) {:position "relative" :content "\"user> \""}]
    [(select/code select/after) {:content "\"\"" :display "block"}]]
   [:.shell
    [(select/code select/before) {:position "relative" :content "\"$ \""}]]
   [:.small {:font-size "0.75em"}]
   #_[:.example-rows
      {:display         "flex"
       :flex-flow       "row wrap"
       :justify-content "space-between"
       :column-gap      "1em"}]
   [:.example-row #_{:flex "1 1 30ch" :font-size "0.75em"}
    {:grid-row "span 2"}]))
