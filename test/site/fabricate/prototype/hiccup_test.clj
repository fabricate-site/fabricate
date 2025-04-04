(ns site.fabricate.prototype.hiccup-test
  (:require [site.fabricate.prototype.hiccup :refer :all]
            [site.fabricate.prototype.html :as html]
            [site.fabricate.prototype.html-test.generators :as html-gen]
            [site.fabricate.prototype.test-utils :refer [with-instrumentation]]
            [hiccup2.core :as hiccup]
            [rewrite-clj.node :as node]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [clojure.test.check :as check]
            [clojure.test.check.clojure-test :refer [defspec]]
            [malli.core :as m]
            [malli.instrument :as mi]
            [clojure.test :as t]))

#_(t/use-fixtures :once with-instrumentation)


(t/deftest metadata-transforms
  (t/testing "Metadata transformation"
    (t/is (= [:meta {:name "meta" :content "something"}]
             (-> {"meta" "something"}
                 seq
                 first
                 ->meta)))
    (t/is (= [:meta {:name "meta" :content "something" :property "some-prop"}]
             (-> {"meta" {:content "something" :property "some-prop"}}
                 seq
                 first
                 ->meta)))
    (t/is
     (=
      #{[:meta
         {:name "site-name" :property "og:site_name" :content "fabricate.site"}]
        [:meta {:name "title" :content "Fabricate" :property "og:title"}]
        [:meta
         {:name "site-title" :content "Fabricate" :property "og:site_name"}]
        [:meta
         {:name     "description"
          :content  "Fabricate: static website generation for Clojure"
          :property "og:description"}]
        [:meta
         {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
        [:meta {:name "locale" :content "en_US" :property "og:locale"}]}
      (into #{}
            (opengraph-enhance opengraph-properties
                               (map ->meta default-metadata-map))))))
  (t/testing "page metadata collection"
    (t/is (= {:icon "src-url.jpg"}
             (lift-metadata [:article {:lang "en" :page/title "Some document"}
                             [:p "some text"]
                             (let [img-url "src-url.jpg"]
                               (with-meta [:img img-url]
                                          {:page/icon  img-url
                                           "some-prop" "a property"}))]
                            {}))))
  (t/testing "html header"
    (t/is
     (=
      [:head [:title "Fabricate | example page"]
       [:link {:rel "stylesheet" :href "/css/normalize.css"}]
       [:link {:rel "stylesheet" :href "/css/remedy.css"}]
       [:link {:rel "stylesheet" :href "/css/patterns.css"}]
       [:link {:rel "stylesheet" :href "/css/extras.css"}]
       [:link {:rel "stylesheet" :href "/css/fabricate.css"}]
       [:meta {:name "title" :content "Fabricate" :property "og:title"}]
       [:meta
        {:name "description" :content "test page" :property "og:description"}]
       [:meta
        {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
       [:meta {:name "locale" :content "en_US" :property "og:locale"}]
       [:meta
        {:name "site-name" :content "fabricate.site" :property "og:site_name"}]
       [:meta
        {:name "site-title" :content "Fabricate" :property "og:site_name"}]
       [:meta {:charset "utf-8"}]
       [:meta {:http-equiv "X-UA-Compatible" :content "IE=edge"}]]
      (doc-header {:title "example page" :description "test page"}))
     "header fn should derive opengraph + metadata from page attributes")))
(def html-newline-gen
  (let [newline-gen
        (gen/vector
         (gen/let [a (gen/vector (gen/one-of [(gen/elements ["\n"])
                                              gen/string-alphanumeric])
                                 5)
                   b (gen/vector (gen/one-of [(gen/elements ["\n\n" "\n"])
                                              gen/string-alphanumeric])
                                 5)
                   c (gen/vector (gen/one-of [(gen/elements ["\n\n" "\n"
                                                             "\n\n\n"])
                                              gen/string-alphanumeric])
                                 5)]
           (apply str (interleave a b c)))
         2
         5)]
    (gen/such-that html/element?
                   (html-gen/hiccup-recursive-elems {:outer-tags html/flow-tags
                                                     :inner-tags
                                                     html/phrasing-tags
                                                     :contents-gen newline-gen})
                   250)))

(t/deftest content-transforms
  (t/testing "Paragraph detection"
    (t/testing ": utilities"
      (t/is (#'site.fabricate.prototype.hiccup/final-match? #"\n\n" "abc\n\n"))
      (t/is (not (#'site.fabricate.prototype.hiccup/final-match?
                  #"\n\n"
                  "abc\n\ndef")))
      (t/is (not (#'site.fabricate.prototype.hiccup/final-match?
                  #"\n\n"
                  "abc\n\ndef\n\n"))))
    (t/is (= [:div [:p "some"] [:p "text"]]
             (parse-paragraphs [:div "some\n\ntext"])))
    (t/is (= [:section] (parse-paragraphs [:section])))
    (t/is (= [[:section]] (parse-paragraphs [[:section]])))
    (t/is (= [:em "text" [:br] "line"] (parse-paragraphs [:em "text\n\nline"])))
    (t/is (= [[:section] [:p "text" [:q "a quote"]] [:section] [:p "more text"]]
             (parse-paragraphs [[:section] [:p "text"] [:q "a quote"] [:section]
                                [:p "more text"]])))
    (t/is (= [:div [:p "some"] [:p "text" [:em "with emphasis"]]]
             (parse-paragraphs [:div "some\n\ntext" [:em "with emphasis"]]
                               {:paragraph-pattern #"\n\n"})))
    (t/is (= '([:p "some text" true 24])
             (parse-paragraphs (list "some text" true 24))))
    (t/is (= [:p [:del [:em [:u "text" [:br] "more text"]]]]
             (parse-paragraphs [:p [:del [:em [:u "text\n\nmore text"]]]]))
          "Paragraphs should be detected at arbitrary levels of nesting")
    (t/is (= [:header [:h1 [:span "text" '([:wbr] "more text")]]]
             (parse-paragraphs [:header
                                [:h1 [:span "text" '([:wbr] "more text")]]]))
          "Paragraphs should not be detected in header elements")
    (t/is (= [:svg {:width 100 :height 100} [:circle {:cx 50 :cy 50 :r 3}]]
             (parse-paragraphs [:svg {:width 100 :height 100}
                                [:circle {:cx 50 :cy 50 :r 3}]]))
          "Paragraph detection should skip SVG elements")
    (t/is (= (list [:p "some text" true 24] [:p "second paragraph"])
             (parse-paragraphs (list "some text" true
                                     24 [:p "second paragraph"])))
          "Flow tags should break out of the prior paragraph")
    (t/is (= (list [:p "some text" true 24] [:div [:p "a div"]])
             (parse-paragraphs [:p "some text" true 24 [:div "a div"]]))
          "Flow tags should break out of the prior paragraph")
    (t/is (= [:h3 "a heading" [:br] "with linebreak"]
             (parse-paragraphs [:h3 "a heading\n\nwith linebreak"]))
          "Paragraph detection should not introduce invalid child elements")
    (t/is (= [:bdi "a bdi" [:br] "with linebreak"]
             (parse-paragraphs [:bdi "a bdi\n\nwith linebreak"]))
          "Paragraph detection should not introduce invalid child elements")
    (t/is (= [:h4 "a heading" [:br] "with linebreak and number" 24]
             (parse-paragraphs [:h4 "a heading\n\nwith linebreak and number"
                                24])))
    (t/is (= [:p "text" [:br] "newline" [:del "more text"]]
             (parse-paragraphs [:p "text\n\nnewline" [:del "more text"]]))
          "Phrasing subtags should persist in enclosing elements")
    (t/is (= [:div [:p [:em "emphasized text"]]]
             (parse-paragraphs [:div [:em "emphasized text"]]))
          "Orphan phrasing elements should be inserted into paragraphs")
    (t/is
     (let [error-form
           [:div [:h6 "Error"]
            [:dl [:dt "Error type"] [:dd [:code "clojure.lang.ExceptionInfo"]]
             [:dt "Error message"]
             [:dd [:code "Unexpected EOF while reading item 1 of list."]]
             [:dt "Error phase"] [:dd [:code ""]] [:dt "Location"]
             [:dd '("Line " [:strong 1] ", " "Columns " [:strong 1 "-" 12])]]
            [:details [:summary "Source expression"]
             [:pre [:code "((+ 2 3)"]]]]]
       (= error-form (parse-paragraphs error-form))))
    (t/is (= [:p "text" [:br] "newline" [:del "more text"]]
             (parse-paragraphs [:p "text\n\n" "newline" [:del "more text"]]))
          "Trailing newlines should still yield <br> elements")
    ;; skip this extreme corner case for now
    #_(t/is (= [[:p] [:div [:p "with div" "and following"]]]
               (parse-paragraphs [:p [:div "with div"] "and following"])))
    (t/is (= '([:p "text" [:br] "newline" [:del "more text"]])
             (parse-paragraphs (list "text\n\n" "newline" [:del "more text"]))))
    (t/is (= [:div [:p "text" [:br] "newline" [:del "more text"]]]
             (parse-paragraphs [:div "text\n\n" "newline" [:del "more text"]])))
    (t/is (= '([:p "text"] [:p "newline" [:del "more text"]])
             (parse-paragraphs (list "text\n\nnewline" [:del "more text"]))))
    (t/is (= [[:p "text" [:br] "newline" [:del "more text"]]]
             (parse-paragraphs ["text\n\n" "newline" [:del "more text"]])))
    (t/is (= [:p {:class "steel"} false]
             (parse-paragraphs [:p {:class "steel"} false] #"\n\n")))
    (t/is
     (=
      [:div {:class "1col"}
       [:p "Linked in the comments on Truyers' post was "
        [:code {:class "ws-normal navy"} "noms"]
        ", a database directly inspired by Git's decentralized and immutable data model, but designed from the ground up to have a better query model and more flexible schema. Unfortunately, it seems to be unmaintained and not ready for prime time. Additionally, for the use case I'm describing, it's unclear how to effectively distribute the configuration data stored in a "
        [:code {:class "ws-normal navy"} "noms"]
        " DB alongside the code that is affected by that configuration in a way that reliably links the two."]]
      (parse-paragraphs
       [:div {:class "1col"} "\n\nLinked in the comments on Truyers' post was "
        [:code {:class "ws-normal navy"} "noms"]
        ", a database directly inspired by Git's decentralized and immutable data model, but designed from the ground up to have a better query model and more flexible schema. Unfortunately, it seems to be unmaintained and not ready for prime time. Additionally, for the use case I'm describing, it's unclear how to effectively distribute the configuration data stored in a "
        [:code {:class "ws-normal navy"} "noms"]
        " DB alongside the code that is affected by that configuration in a way that reliably links the two."])))
    (t/is
     (= [:div {:class "row"}
         [:p "orphan text" [:em "with emphasis added"] "and"] [:p "linebreak"]]
        (parse-paragraphs [:div {:class "row"} "orphan text"
                           [:em "with emphasis added"] "and\n\nlinebreak"])))
    (t/is (= (list [:h3 "header"]
                   [:p "some preliminary text"]
                   [:p " followed by a double linebreak, "
                    [:code "with inline code"]
                    " and more text following, in the same paragraph"]
                   [:h3 "and another header"])
             (parse-paragraphs
              (list [:h3 "header"]
                    "some preliminary text\n\n followed by a double linebreak, "
                    [:code "with inline code"]
                    " and more text following, in the same paragraph"
                    [:h3 "and another header"]))))
    (t/is
     (=
      '([:h2 "a header"]
        [:p "Some starting text, with some code thrown in. "]
        [:p "This the code: " [:code "some code"]
         "There's more text afterwards."])
      (parse-paragraphs
       (list
        [:h2 "a header"]
        "\n\nSome starting text, with some code thrown in. \n\nThis the code: "
        [:code "some code"]
        "There's more text afterwards.\n\n"))))
    (t/is (= [:div [:p "orphan text" [:em "with emphasis added"] "and"]
              [:p "linebreak"]
              (list [:p "and list contents, "] [:p "also with linebreak"])]
             (parse-paragraphs
              [:div "orphan text" [:em "with emphasis added"] "and\n\nlinebreak"
               (list "and list contents, \n\nalso with linebreak")]))
          "List elements should be processed in place and in order")
    (let [expr-form   [:pre [:code {"class" "language-clojure"}] "(+ 3 4)"]
          expr-result 7
          pre-parsed  [:div "a section" (list expr-form expr-result)]
          parsed      (parse-paragraphs pre-parsed)
          tree        (tree-seq sequential? identity parsed)]
      (t/is
       (< (.indexOf tree expr-form) (.indexOf tree expr-result))
       "After paragraph detection, expression results should come after the form"))
    (t/is (= [:p "some text" [:br] "with newlines"]
             (parse-paragraphs [:p "some text\n\nwith newlines"])))
    (t/is (= '([:p "some text" [:br] "with newlines"])
             (parse-paragraphs (list [:p "some text\n\nwith newlines"]))))
    (t/is (= [:section [:p "some text"] [:p "with newlines"]]
             (parse-paragraphs [:section "some text\n\nwith newlines"])))
    (t/is
     (=
      [:img
       {:src
        "https://upload.wikimedia.org/wikipedia/commons/9/90/Pterodroma_mollis_light_morph_-_SE_Tasmania_2019.jpg"}
       [:p "with newlines"]]
      (parse-paragraphs
       [:img
        {:src
         "https://upload.wikimedia.org/wikipedia/commons/9/90/Pterodroma_mollis_light_morph_-_SE_Tasmania_2019.jpg"}
        [:p "with newlines"]])))
    (t/is
     (=
      [:figure
       [:img
        {:src
         "https://upload.wikimedia.org/wikipedia/commons/9/90/Pterodroma_mollis_light_morph_-_SE_Tasmania_2019.jpg"}]
       [:figcaption "soft-plumaged petrel"]]
      (parse-paragraphs
       [:figure
        [:img
         {:src
          "https://upload.wikimedia.org/wikipedia/commons/9/90/Pterodroma_mollis_light_morph_-_SE_Tasmania_2019.jpg"}]
        [:figcaption "soft-plumaged petrel"]])))))

(defspec paragraph-detection-output
         (prop/for-all [html-elem html-newline-gen]
                       (or (m/validate html/atomic-element html-elem)
                           (html/element? (parse-paragraphs html-elem)))))
