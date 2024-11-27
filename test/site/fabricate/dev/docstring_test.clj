(ns site.fabricate.dev.docstring-test
  (:require [site.fabricate.dev.docstrings :as docstrings]
            [clojure.test :as t]))


(t/deftest notation
  (t/testing "Bracket notation"
    (t/is (= "Key term"
             (:contents (docstrings/extract-bracket-text "[[Key term]]")))
          "Plaintext should be returned as a term")
    (t/is (= "Key term|term"
             (:contents (docstrings/extract-bracket-text "[[Key term|term]]")))
          "Plaintext with alias should be returned as a term")
    (t/is (= "'symbol"
             (:contents (docstrings/extract-bracket-text "[['symbol]]"))))
    (t/is (= (quote 'symbol) (:symbol (docstrings/parse-bracket "[['symbol]]")))
          "Quoted symbols should be returned from parse brackets")
    (t/is (= (quote #'docstrings/parse-bracket)
             (:var (docstrings/parse-bracket "[[#'docstrings/parse-bracket]]")))
          "Vars should be returned unresolved from parse brackets")
    (t/is (= :keyword (:keyword (docstrings/parse-bracket "[[:keyword]]")))))
  (t/testing "Backtick notation"))
