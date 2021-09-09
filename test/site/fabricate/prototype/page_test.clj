(ns site.fabricate.prototype.page-test
  (:require [site.fabricate.prototype.page :refer :all ]
            [hiccup2.core :as hiccup]
            [clojure.test :as t]))

(t/deftest page-creation
  (t/testing "html helper fns"

    (t/is (= "<em>help</em>" (str (hiccup/html (em "help"))))
          "emphasis should be added")
    (t/is (= "<em>help</em>" (str (hiccup/html (em "help"))))
          "emphasis should be added")))

(t/deftest metadata
  (t/testing "Metadata transformation"
    (t/is (= [:meta {:name "meta" :content "something"}]
             (-> {"meta" "something"} seq first ->meta)))

    (t/is (= [:meta {:name "meta" :content "something"
                     :property "some-prop"}]
             (-> {"meta" {:content "something"
                          :property "some-prop"}}
                 seq first ->meta)))))

(t/deftest transforms

  (t/testing "Paragraph detection"
    (t/is (= [:div [:p "some"] [:p "text"]]
             (detect-paragraphs [:div "some\n\ntext"] #"\n\n")))

    (t/is (=
           [:div [:p "some"] [:p "text" [:em "with emphasis"]]]
           (detect-paragraphs [:div "some\n\ntext" [:em "with emphasis"]]
                              #"\n\n")))

    #_ (t/is (= [:div] (detect-paragraphs [:div " "] #"\n\n"))
             "Whitespace-only text should not be tokenized into paragraphs")

    (t/is
     (=
      [:div
       {:class "1col"}
       [:p "Linked in the comments on Truyers' post was "
        [:code {:class "ws-normal navy"} "noms"]
        ", a database directly inspired by Git's decentralized and immutable data model, but designed from the ground up to have a better query model and more flexible schema. Unfortunately, it seems to be unmaintained and not ready for prime time. Additionally, for the use case I'm describing, it's unclear how to effectively distribute the configuration data stored in a "
        [:code {:class "ws-normal navy"} "noms"]
        " DB alongside the code that is affected by that configuration in a way that reliably links the two."]]
      (detect-paragraphs
       [:div
        {:class "1col"}
        "\n\nLinked in the comments on Truyers' post was "
        [:code {:class "ws-normal navy"} "noms"]
        ", a database directly inspired by Git's decentralized and immutable data model, but designed from the ground up to have a better query model and more flexible schema. Unfortunately, it seems to be unmaintained and not ready for prime time. Additionally, for the use case I'm describing, it's unclear how to effectively distribute the configuration data stored in a "
        [:code {:class "ws-normal navy"} "noms"]
        " DB alongside the code that is affected by that configuration in a way that reliably links the two."]
       #"\n\n")))


    (t/is
     (=
      [:div
       {:class "row"}
       [:p "orphan text" [:em "with emphasis added"] "and"]
       [:p "linebreak"]]
      (detect-paragraphs
       [:div
        {:class "row"}
        "orphan text"
        [:em "with emphasis added"]
        "and\n\nlinebreak"] #"\n\n"))))


  (t/testing "Sectionizer"
    (t/is (=
           [:article
            [:section
             [:p "Some text"]
             [:p "Some more text"]]]
           (into [:article]
                 sectionize-contents
                 [[:p "Some text"]
                  [:p "Some more text"]])))

    (t/is (=
           [:article
            [:section
             [:p "Some text"]
             [:p "Some more text"]]
            [:section
             [:p [:q "a quote"]]]]
           (into [:article]
                 sectionize-contents
                 [[:p "Some text"]
                  [:p "Some more text"]
                  [:section]
                  [:q "a quote"]])))

    (t/is (= [:article
              [:section
               [:p "text"
                [:q "a quote"]]]
              [:section
               [:p "more text"]]]
             (into [:article]
                   sectionize-contents
                   [[:section]
                    [:p "text"]
                    [:q "a quote"]
                    [:section]
                    [:p "more text"]])))

    (t/is (= [:article
              [:section
               [:p "text"
                [:q "a quote"]]]
              [:section
               [:p "more text"]]]
             (into [:article]
                   sectionize-contents
                   [[:p "text"]
                    [:q "a quote"]
                    [:section]
                    [:p "more text"]])))

    (t/is (= [:article
              [:section
               [:p "text"
                [:q "a quote"]]]
              [:section
               [:header [:h1 "section header"]]
               [:p "more text"]]]
             (into [:article]
                   sectionize-contents
                   [[:section]
                    [:p "text"]
                    [:q "a quote"]
                    [:section
                     [:header [:h1 "section header"]]]
                    [:p "more text"]])))

    (t/is (= [:article
              [:section
               [:p "text"
                [:q "a quote"]]]
              [:section
               [:div {:class "subsection"}
                [:h3 "Subsection 1"]
                [:p "subsection 1 text"]]
               [:div {:class "subsection"}
                [:h3 "Subsection 2"]
                [:p "subsection 2 text"]]]]
             (into [:article]
                   sectionize-contents
                   [[:p "text"]
                    [:q "a quote"]
                    [:section]
                    :next
                    [:div {:class "subsection"}]
                    [:h3 "Subsection 1"]
                    [:p "subsection 1 text"]
                    :next
                    [:div {:class "subsection"}]
                    [:h3 "Subsection 2"]
                    [:p "subsection 2 text"]]))))

  (t/testing "Next Processing"
    (t/is
     (=
      [:div [:p "test" [:a "something"]] [:div "something else"]]
      (process-nexts [:div :next [:p] "test" [:a "something"] :next [:div] "something else"])))
    (t/is
     (=
      [:div [:p "test" [:a "something"]] [:div "something else"]]
      (process-nexts [:div [:p "test" [:a "something"]] [:div "something else"]])))
    (t/is
     (=
      [:div [:p "test"]]
      (process-nexts [:div [:p "test"]])))
    (t/is
     (=
      [:div]
      (process-nexts [:div])))))
