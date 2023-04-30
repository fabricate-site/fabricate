(ns site.fabricate.styles
  (:require
   [garden.core :as garden]
   [garden.selectors :as select]
   [garden.stylesheet :as stylesheet]))

(def ^:private roboto-flex-import-url
  "https://fonts.googleapis.com/css2?family=Roboto+Flex:opsz,wdth,wght@8..144,85..151,100..1000&display=swap")

(def doc-colors
  {:white           "#fcfcfc"
   :sky             "#dfebe6"
   :steel           "#8b94a1"
   :dark-steel      "#454858"
   :black           "#11130f"
   :dark-green      "#1f2c11"
   :rust            "#683223"
   :cobalt          "#264273"
   :golden          "#ceaf6d"
   :wood            "#917766"
   :light-wood      "#d0c5b6"
   }
  )

(defn card-block [contents]
  [:div {:class "title-card"}
   contents])

(def docs
  (list
   (stylesheet/at-import "https://fonts.googleapis.com/css2?family=Sono:wght,MONO@200..800,0..1&display=swap")

   (stylesheet/at-import roboto-flex-import-url)
   (stylesheet/at-font-face
    {:font-family "'Katahdin Round'"
     :src ["url('../fonts/KatahdinRound-Bold.woff2')"]})
   (stylesheet/at-font-face
    {:font-family "Junction"
     :src ["url('../fonts/JunctionVariable-Regular.woff2')"]})

   [:article {:font-family "Junction,sans-serif"
              :font-optical-sizing "auto"
              :font-size "min(4vmin, 20px)"
              :font-variation-settings "'wdth' 91"
              :color (doc-colors :white)
              :background-color (doc-colors :dark-green)
              #_ #_ :margin-left "1vw"
              :line-height "1.29em"
              :display "grid"
              :grid-template-columns "repeat(12, minmax(3px, 1fr))"
              :grid-column-gap "0.5rem"
              :grid-row-gap "0.5em"}]
   [:aside {:grid-column "5 / 12"
            :font-size "0.8em" :font-weight "400"}]

   [:.card {:background-color (doc-colors :light-wood)
            :color (doc-colors :black)
            :padding-top "1.5em"
            :margin-bottom "2.5em"
            :padding-left "1em"
            :padding-right "1em"}]
   [:.title-card {:font-family "'Katahdin Round', sans-serif"
                  :grid-column "2 / span 3"
                  :text-transform "uppercase"
                  :background-color (doc-colors :rust)
                  :color (doc-colors :golden)
                  :transform "rotate(-3.5deg)"
                  :box-shadow "7px 7px black"
                  :padding "0.75em"
                  :margin-bottom "-1.5em"
                  #_:drop-shadow #_ ""}]

   #_[(select/aside select/before)
      {:content "\"📝\"" :left "0"
       :position "absolute" :grid-column "5"}]
   [:h1 :h2 :h3 :h4 :h5 :h6
    :header
    {:font-weight "900"
     :grid-column "2 / 10"
     :font-family "Junction,sans-serif"
     :margin-bottom "0.25em"
     :margin-top "0em"}]
   [:body {:background-color (doc-colors :dark-green)}]
   [:footer {:font-family "Junction, sans-serif"
             :font-weight "500"}]
   [:p {:grid-column "2 / 10"
        :margin-bottom "0.5em"
        :padding "0px"
        :margin-top "0em"
        :text-align "justify"
        :text-justify "inter-word"
        :text-align-last "left"
        :hyphens "auto"}]
   [:div :section {:grid-column "2 / 12"
                   :margin-bottom "0em"
                   :margin-top "0em"}]
   [:ul :ol {:grid-column "2 / 11"
             :margin-top "0em"
             :margin-bottom "0em"}
    [:li {:margin-bottom "0.4em"
          :margin-left "-2rem"
          :padding-left "0em"}]]
   [:figure {:margin-left "0em"}]
   [:figure :blockquote {:grid-column "2 / 10"}]
   [:blockquote {:margin-left "2em" :margin-right "2em"}]
   [:code {:font-family "'Sono', monospace"
           :font-weight "500"
           :font-size "0.92em"
           :white-space "pre-wrap"
           :background-color (doc-colors :white)}]
   [:pre {:background-color (doc-colors :white)
          :color (doc-colors :black)
          :white-space "pre-wrap"
          :grid-column "2 / 12"
          :margin-bottom "0.5em"
          :font-weight "450"
          :line-height "1.45rem"
          :margin-top "0em"
          :font-family "'Sono', monospace"}
    [:ins {:text-decoration "none" :background-color "#ADC3A2"}]
    [(select/ins select/before) {:position "relative"
                                 :background-color "#EEE"
                                 :font-weight "700"
                                 :content "\"+\""}]
    [(select/ins select/after) {:content "\"\"" :display "block"}]]
   [:dl {:display "grid" :grid-column-gap "0.5em"
         :grid-row-gap "0.65em"}
    [:dt {:grid-column "1 / 3 !important" :font-weight "600"}]
    [:dd {:grid-column "3 !important"
          :margin-left "0em" :min-width "25ch"}]]
   [:dl [:dl {:display "block"}
         [:dt {:grid-column "auto"}]
         [:dd {:grid-column "auto"}]]]

   (stylesheet/at-media
    {:max-width "900px"}
    [:p :ul :ol {:grid-column "1 / 10"}]
    [:div {:grid-column "1 / 13"}]
    [:pre :figure :blockquote {:grid-column "2 / 13"}]
    [:h1 :h2 :h3 :h4 :h5 :h6
     :header
     {:grid-column "1 / 13"}])
   [:.xl-text {:font-family "'Katahdin Round', sans-serif"
               :font-size "2.5em" :line-height "1em"
               :text-transform "uppercase"
               :color (doc-colors :golden)}]
   [:.l-text {:font-family "'Katahdin Round', sans-serif"
              :font-size "1.5em" :line-height "1em"
              :text-transform "uppercase"}]
   [:.language-clojure {:font-family "'Sono', monospace"}
    [:.keyword {:color "#7f2c28"}]
    [:.string {:color "#7a7533"}]
    [:.symbol {:color "#4f2c28"}]
    [:.comment {:font-weight "300"
                :color "#555"}]
    [:.uneval {:font-weight "300"
               :color "#555"}]]
   [:a {:margin-bottom "0em"
        :text-decoration "none"
        :color "#337E80"}]
   [:.repl [(select/code select/before) {:position "relative"
                                         :content "\"user> \""}]

    [(select/code select/after) {:content "\"\"" :display "block"}]]
   [:.shell [(select/code select/before) {:position "relative" :content "\"$ \""}]]
   [:.small {:font-size "0.75em"}]
   #_[:.example-rows {:display "flex"
                      :flex-flow "row wrap"
                      :justify-content "space-between"
                      :column-gap "1em"}]
   [:.example-row #_{:flex "1 1 30ch" :font-size "0.75em"}
    {:grid-row "span 2"}]))
