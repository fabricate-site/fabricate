(ns site.fabricate.prototype.read.grammar-test
  (:require [site.fabricate.prototype.read.grammar  :refer :all]
            [clojure.test :as t]))


(t/deftest parser

  (t/is (vector? (form "✳=abcd🔚 some text")))
  (t/is (vector? (form "✳=(+ 3 4 5)🔚 some text")))
  (t/is (vector? (form "✳=(my.ns/fn  22)🔚 some text"))))
