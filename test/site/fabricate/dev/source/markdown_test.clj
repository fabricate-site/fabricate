(ns site.fabricate.dev.source.markdown-test
  (:require [site.fabricate.dev.source.markdown :as md]
            [clojure.test :as t]))


(t/deftest parsing-extensions
  (let [test-txt    "[[basic wikilink]]"
        test-hiccup (md/md->hiccup test-txt)]
    (t/testing "wikilinks"
      (t/is (some? test-hiccup)
            "Basic parsing should return values without errors")
      (t/is (= "[[basic wikilink]]" (get-in test-hiccup [2 2 1 :reference]))
            "Link text with brackets should be retained")
      (t/is (= "basic wikilink" (get-in test-hiccup [2 2 2]))
            "Link contents should be taken out of brackets in body text"))))
