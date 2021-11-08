(ns site.fabricate.prototype.read.grammar-test
  (:require [site.fabricate.prototype.read.grammar  :refer :all]
            [instaparse.core :as insta]
            [clojure.test :as t]))


(t/deftest parser

  (t/testing "simple forms"
    (t/is (vector? (template "✳=abcd🔚 some text")))
    (t/is (vector? (template "✳=(+ 3 4 5)🔚 some text")))
    (t/is (vector? (template "✳=(my.ns/fn  22)🔚 some text"))))

  (t/testing "ambiguity"
    (doseq [f ["./pages/finite-schema-machines.html.fab"
               "./pages/fabricate.html.fab"
               "./README.md.fab"]]
      (t/is (= 1 (count (insta/parses template (slurp f))))))))
