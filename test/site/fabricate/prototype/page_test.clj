(ns site.fabricate.prototype.page-test
  (:require [site.fabricate.prototype.page :refer :all ]
            [site.fabricate.prototype.html :as html]
            [site.fabricate.prototype.html-test.generators :as html-gen]
            [hiccup2.core :as hiccup]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [clojure.test.check :as check]
            [clojure.test.check.clojure-test :refer [defspec]]
            [malli.core :as m]
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
                 seq first ->meta)))
    (t/is
     (= '([:meta {:name "title", :content "Fabricate"}]
          [:meta {:name "description", :content "Fabricate: static website generation for Clojure", :property "og:description"}]
          [:meta {:name "viewport", :content "width=device-width, initial-scale=1.0, user-scalable=no"}]
          [:meta {:name "HTTP Attributes", :charset "utf-8", :http-equiv "X-UA-Compatible", :content "IE=edge,chrome=1"}])
      (opengraph-enhance {"description" "og:description"}
                         (map ->meta default-metadata))))

    ))

(def html-newline-gen
  (let [newline-gen
        (gen/vector
         (gen/let
             [a (gen/vector
                 (gen/one-of
                  [(gen/elements ["\n"]) gen/string-alphanumeric])
                 5)
              b (gen/vector
                 (gen/one-of
                  [(gen/elements ["\n\n" "\n"]) gen/string-alphanumeric])
                 5)
              c (gen/vector
                 (gen/one-of
                  [(gen/elements ["\n\n" "\n" "\n\n\n"]) gen/string-alphanumeric])
                 5)]
             (apply str (interleave a b c)))
         2 5)]
    (gen/such-that
     html/element?
     (html-gen/hiccup-recursive-elems
      {:outer-tags html/flow-tags
       :inner-tags html/phrasing-tags
       :contents-gen newline-gen})
     250)))



(comment

  (hiccup/html [:em "some text\n\nwith double linebreak"])

  (parse-paragraphs [:em "some text\n\nwith newlines"] #"\n\n")


  (parse-paragraphs
   [:p {:class "steel"} false]
   #"\n\n")

  (html/phrasing?
   [:p {:class "steel"} false])



  (parse-paragraphs
   (first
    (get-in
     (check/quick-check
      10
      (prop/for-all
       [html-elem html-newline-gen]
       (or (m/validate html/atomic-element html-elem)
           (html/element? (parse-paragraphs html-elem #"\n\n")))))
     [:shrunk :smallest])))

  )

(comment
  (html/phrasing? false)

  )

(t/deftest transforms

  (t/testing "Paragraph detection"
    (t/is (= [:div [:p "some"] [:p "text"]]
             (parse-paragraphs [:div "some\n\ntext"])))

    (t/is (=
           [:div [:p "some"] [:p "text" [:em "with emphasis"]]]
           (parse-paragraphs [:div "some\n\ntext" [:em "with emphasis"]]
                             {:paragraph-pattern #"\n\n"})))

    (t/is (= [:p "some text" true 24]
             (parse-paragraphs
              (list "some text" true 24))))

    (t/is
     (= (list [:p "some text" true 24] [:p "second paragraph"])
        (parse-paragraphs
         (list "some text" true 24 [:p "second paragraph"])))
     "Flow tags should break out of the prior paragraph")

    (t/is
     (= (list [:p "some text" true 24] [:div [:p "a div"]])
        (parse-paragraphs
         [:p "some text" true 24 [:div "a div"]]))
     "Flow tags should break out of the prior paragraph")

    (t/is
     (= [:h3 "a heading" [:br] "with linebreak"]
        (parse-paragraphs
         [:h3 "a heading\n\nwith linebreak"]))
     "Paragraph detection should not introduce invalid child elements")

    (t/is
     (= [:bdi "a bdi" [:br] "with linebreak"]
        (parse-paragraphs
         [:bdi "a bdi\n\nwith linebreak"]))
     "Paragraph detection should not introduce invalid child elements")



    (t/is
     (= [:p {:class "steel"} false]
        (parse-paragraphs
         [:p {:class "steel"} false]
         #"\n\n")))

    (t/is (= [:div] (parse-paragraphs [:div " "]))
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
      (parse-paragraphs
       [:div
        {:class "1col"}
        "\n\nLinked in the comments on Truyers' post was "
        [:code {:class "ws-normal navy"} "noms"]
        ", a database directly inspired by Git's decentralized and immutable data model, but designed from the ground up to have a better query model and more flexible schema. Unfortunately, it seems to be unmaintained and not ready for prime time. Additionally, for the use case I'm describing, it's unclear how to effectively distribute the configuration data stored in a "
        [:code {:class "ws-normal navy"} "noms"]
        " DB alongside the code that is affected by that configuration in a way that reliably links the two."])))


    (t/is
     (=
      [:div
       {:class "row"}
       [:p "orphan text" [:em "with emphasis added"] "and"]
       [:p "linebreak"]]
      (parse-paragraphs
       [:div
        {:class "row"}
        "orphan text"
        [:em "with emphasis added"]
        "and\n\nlinebreak"])))


    (t/is
     (=  [:p "some text" [:br] "with newlines"]
         (parse-paragraphs [:p "some text\n\nwith newlines"])))

    (t/is
     (=  [:section [:p "some text"] [:p "with newlines"]]
         (parse-paragraphs [:section "some text\n\nwith newlines"])))

    )


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

(defspec paragraph-detection-output
  (prop/for-all
       [html-elem html-newline-gen]
       (or (m/validate html/atomic-element html-elem)
           (html/element? (parse-paragraphs html-elem)))))
