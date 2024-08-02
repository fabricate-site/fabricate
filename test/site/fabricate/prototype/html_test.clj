(ns site.fabricate.prototype.html-test
  (:require [site.fabricate.prototype.html :as html :refer :all]
            [site.fabricate.prototype.schema :as schema]
            [malli.core :as m :refer [validate]]
            [malli.error :as me]
            [malli.generator :as mg]
            [clojure.set :as set]
            [clojure.pprint :as pprint]
            [clojure.test :as t]))

(defmethod t/assert-expr 'valid-schema?
  [msg form]
  `(let [schema#      ~(nth form 1)
         form#        (m/form schema#)
         data#        ~(nth form 2)
         result#      (m/validate schema# data#)
         schema-name# (last form#)]
     (t/do-report
      {:type     (if result# :pass :fail)
       :message  ~msg
       :expected (str (with-out-str (pprint/pprint data#))
                      " conforms to schema for "
                      schema-name#)
       :actual   (if (not result#) (m/explain schema# data#) result#)})
     result#))

(def example-forms
  "Some forms used to test the validity of the HTML schema"
  {:a       [:a {:href "http://www.archive.org"} "a link"]
   :data    [:data {:value "0311ab"} "A sample post"]
   :del     [:del "some deleted text"]
   :dl      [:dl {:id "definitions"} [:dt ":dl - Definition List"]
             [:dd "A HTML element with a list of definitions"]]
   :figure  [:figure [:figcaption "a picture"]
             [:img {:src "/some-picture.png"}]]
   :ul      [:ul [:li "some text"] [:li "more text"]]
   :bdo     [:bdo {:dir "rtl"} "right to left text"]
   :time    [:time {:datetime "2020-12-31"}]
   :img     [:img {:src "/sample.jpg"}]
   :head    [:head [:title "a page"]
             [:script {:type "text/javascript" :src "/intro.js"}]
             [:style "h1 {size: 3rem}"]]
   :span    [:span [:img {:src "/sample.jpg"}]]
   #_#_:script [:script {:type "text/javascript" :src "code.js"}]
   :q       [:q {:cite "Anonymous"} "If you can't convince, confuse!"]
   :script  [:script {:src "/resources/klipse.js" :type "text/javascript"} ""]
   #_#_:wbr [:wbr]
   :hr      [:hr]
   :br      [:br]
   :wbr     [:wbr]
   :abbr    [:abbr {:title "ACME Corporation"} "ACME"]
   :ol      [:ol [:li "item"] [:li "item2"]]
   :hgroup  [:hgroup [:h1 "big header"] [:h4 "small header"]]
   :link    [:link {:rel "stylesheet" :href "/main.css"}]
   :details [:details [:summary [:span "summarized text"]] [:p "text"]]
   :table   [:table [:caption "an example table"] [:colgroup [:col]]
             [:thead [:tr [:td "label"]]] [:tbody [:tr [:td "a cell"]]]]
   :article [:article [:section "something"]]})

(t/deftest schema
  (t/testing "content schemas"
    (t/is (valid-schema? (#'site.fabricate.prototype.html/->hiccup-schema
                          :p
                          global-attributes
                          [:* atomic-element])
                         [:p {:id "something"} "text in a paragraph"]))
    (t/is (valid-schema? (schema/subschema html ::html/p)
                         [:p "something"
                          [:a {:href "https://link.com"} "text"]])
          "Phrasing subtags should be respected.")
    (t/is (valid-schema? (schema/subschema html "a-phrasing")
                         [:a {:href "https://something.com"}
                          [:ins "something"
                           [:del "something" [:em "something else"]]]])
          "Phrasing subtags should be respected.")
    (t/is (valid-schema? (schema/subschema html "ins-phrasing")
                         [:ins [:ins [:ins [:em "text"]]]])
          "Phrasing subtags should be respected")
    (t/is (valid-schema? (schema/subschema html "ins-phrasing")
                         [:ins [:ins "text"]])
          "Phrasing subtags should be respected")
    (t/is (valid-schema? (schema/subschema html "del-phrasing")
                         [:del [:em "text"]])
          "Phrasing subtags should be respected")
    (t/is (valid-schema? (schema/subschema html ::html/em)
                         [:em [:ins [:ins [:em "text"]]]])
          "Phrasing subtags should be respected")
    (t/is (valid-schema? (schema/subschema html ::html/em)
                         [:em [:a {:href "https://archive.org"}] "something"])
          "Phrasing subtags should be respected")
    (t/is (not (m/validate (schema/subschema html "ins-phrasing")
                           [:ins [:ins [:ins [:p "text"]]]])))
    (t/is (valid-schema? (schema/subschema html "a-phrasing")
                         [:a {:href "https://example.com"} "link" [:em "text"]])
          "Phrasing subtags should be respected")
    (t/is (valid-schema? (schema/subschema html ::html/p)
                         [:p "text" [:img {:src "/picture.jpg"}]]))
    (t/is (valid-schema? (schema/subschema html ::html/em)
                         [:em "text" [:br] "more text"]))
    (t/is (valid-schema? (schema/subschema html ::html/em)
                         [:em {:id "something"} "text" "more text"]))
    (doseq [elem (set/union flow-tags phrasing-tags heading-tags)]
      (t/testing (str "schema for element: <" (name elem) ">")
        (let [data   (get example-forms elem [elem "sample string"])
              schema (schema/subschema html
                                       (ns-kw 'site.fabricate.prototype.html
                                              elem))]
          (t/is (valid-schema? schema data)))))
    (t/is (palpable? [:p "text"]))
    (t/is (not (palpable? [:p])))
    (t/is (valid-schema? (schema/subschema html ::html/element)
                         [:div [:div [:div [:p "text"]]]])))
  (t/testing "example forms"
    (doseq [[k v] example-forms]
      (let [schema (schema/subschema html
                                     (ns-kw 'site.fabricate.prototype.html k))]
        (t/testing (str "schema for element: <" (symbol k) ">")
          (t/is (valid-schema? schema v))))))
  (t/testing "page structure"
    (doseq [[tag element] example-forms]
      (let [example-page [:html [:head] [:body element]]]
        (t/testing (str "schema for element: <" (symbol tag) ">")
          (when (not= :head tag) (t/is (valid-schema? html example-page)))))))
  (comment
    (map (fn [[k v]] [k (valid-schema? htmls v)]) example-forms))
  (t/testing "atomic elements"
    (t/is (m/validate global-attributes {:class "a" :href "http://google.com"}))
    (t/is (m/validate global-attributes
                      {:title "some page" :href "/relative-page.html"}))))


(t/deftest processing
  (t/testing "Parse + unparse for elements"
    (let [unparse-element (m/unparser element)]
      (doseq [[tag elem] example-forms]
        (if (not= :head tag)
          (t/is
           (= elem
              (-> elem
                  parse-element
                  unparse-element))
           (str "<" (name tag) "> should parse and unparse correctly")))))))
