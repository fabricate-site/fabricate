(ns site.fabricate.styles
  (:require
   [garden.core :as garden]
   [garden.selectors :as select]
   [garden.stylesheet :as stylesheet]))

(def docs
  (list
   (stylesheet/at-import "https://fonts.googleapis.com/css2?family=Spline+Sans:wght@300..700&display=swap")
   [:article {:font-family "'Spline Sans', sans-serif"
              :font-size "22px"
              :color "#222"
              :margin-left "1vw"
              :line-height "1.45em"
              :max-width "70ch"}]
   [:aside {:margin-left "1.75vw" :font-size "0.8em" :font-weight "400"}]
   [(select/aside select/before) {:content "\"🤔\"" :left "0" :position "absolute" :padding "0.5em" :margin-left "1.5vw"}]
   [:h1 :h2 :h3 :h4 :h5 :h6 {:font-weight "700"}]
   [:html {:background-color "#E2DED6"}]
   [:pre {:background-color "#EEE" :white-space "pre-wrap"}
    [:ins {:text-decoration "none" :background-color "#ADC3A2"}]
    [(select/ins select/before) {:position "relative"
                                 :background-color "#EEE"
                                 :font-weight "700"
                                 :content "\"+\""}]
    [(select/ins select/after) {:content "\"\"" :display "block"}]]
   [:a {:margin-bottom "0em"
        :text-decoration "none"
        :color "#337E80"}]
   [:.repl [(select/code select/before) {:position "relative"
                                         :content "\"user> \""}]

    [(select/code select/after) {:content "\"\"" :display "block"}]]
   [:.shell [(select/code select/before) {:position "relative" :content "\"$ \""}]]
   [:.small {:font-size "0.75em"}]
   [:.example-rows {:display "flex"
                    :flex-flow "row wrap"
                    :justify-content "space-between"
                    :column-gap "1em"}]
   [:.example-row {:flex "1 1 30ch" :font-size "0.75em"}]))
