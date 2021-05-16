(ns site.fabricate.page-test
  (:require [site.fabricate.page :refer :all ]
            [hiccup2.core :refer [html]]
            [clojure.test :as t]))

(t/deftest page-creation
  (t/testing "html helper fns"

    (t/is (= "<em>help</em>" (str (html (em "help"))))
          "emphasis should be added")
    (t/is (= "<em>help</em>" (str (html (em "help"))))
          "emphasis should be added")))
