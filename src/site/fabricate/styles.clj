(ns site.fabricate.styles
  (:require
   [garden.core :as garden]
   [garden.selectors :as select]
   [garden.stylesheet :as stylesheet]))

(def docs
  (list
   (stylesheet/at-import "https://fonts.googleapis.com/css2?family=Overpass+Mono:wght@300..700&family=Overpass:ital,wght@100..700&display=swap")
   (stylesheet/at-import "https://fonts.googleapis.com/css2?family=Spline+Sans:wght@300..700&display=swap")
   (stylesheet/at-font-face {:font-family "Katahdin"
                             :src "url(/media/KatahdinRound-Bold.woff2) format('woff')"})
   (stylesheet/at-font-face
    {:font-family "Newsreader"
     :src "url(/media/Newsreader-Italic[opsz,wght].woff2) format('woff')"})
   (stylesheet/at-font-face {:font-family "Newsreader"
                             :src "url(/media/Newsreader[opsz,wght].woff2) format('woff')"})
   [:article {:font-family "'Overpass', sans-serif"
              :font-size "min(4vmin, 22px)"
              :color "#222"
              #_ #_ :margin-left "1vw"
              :line-height "1.29em"
              :display "grid"
              :grid-template-columns "repeat(12, minmax(5px, 1fr))"
              :grid-column-gap "1rem"
              :grid-row-gap "0.5em"}]
   [:aside {:margin-left "1.75vw" :font-size "0.8em" :font-weight "400"}]
   [(select/aside select/before) {:content "\"🤔\"" :left "0" :position "absolute" :padding "0.5em" :margin-left "1.5vw"}]
   [:h1 :h2 :h3 :h4 :h5 :h6
    {:font-weight "900"
     :font-family "'Overpass', sans-serif"
     :margin-bottom "0.25em"
     :margin-top "0em"}]
   [:body {:background-color "#E2DED6"}]
   [:p {:grid-column "4 / 9"
        :margin-bottom "0.5em"
        :padding "0px"
        :margin-top "0em"
        :hyphens "auto"}]
   [:div {:grid-column "3 / 10"
          :margin-bottom "0em"
          :margin-top "0em"}]
   [:ul :ol {:grid-column "4 / 9"
             :margin-top "0em"
             :margin-bottom "0em"}
    [:li {:margin-bottom "0.5em"}]]
  (stylesheet/at-media
   {:max-width "750px"}
   [:p :ul :ol :pre {:grid-column "2 / 11"}]
   [:div {:grid-column "1 / 12"}])
   [:code {:font-family "'Overpass Mono', monospace"
           :background-color "#EEE"}]
   [:pre {:background-color "#EEE" :white-space "pre-wrap"
          :grid-column "3 / 10"
          :margin-bottom "0.5em"
          :margin-top "0em"
          :font-family "'Overpass Mono', monospace"}
    [:ins {:text-decoration "none" :background-color "#ADC3A2"}]
    [(select/ins select/before) {:position "relative"
                                 :background-color "#EEE"
                                 :font-weight "700"
                                 :content "\"+\""}]
    [(select/ins select/after) {:content "\"\"" :display "block"}]]
   [:.xl-text {:font-family "Katahdin, sans-serif"
               :font-size "2.5em" :line-height "1em"
               :text-transform "uppercase"}]
   [:.l-text {:font-family "Katahdin, sans-serif"
              :font-size "1.5em" :line-height "1em"
              :text-transform "uppercase"}]
   [:.language-clojure
    {:font-family "Overpass Mono, sans-serif"}
    [:.keyword {:color "#7f2c28"}]
    [:.string {:color "#7a7533"}]]
   [:figure {:margin-left "0.25vw"
             :margin-right "0.25vw"
             :margin-top "0.5em"
             :margin-bottom "0.5em"}]
   [:blockquote {:margin-left "0.25em"
                 :margin-right "0.25em"}]
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
