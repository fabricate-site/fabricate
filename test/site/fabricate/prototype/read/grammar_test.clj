(ns site.fabricate.prototype.read.grammar-test
  (:require [site.fabricate.prototype.read.grammar  :refer :all]
            [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.test :as t]))

(t/deftest parser

  (t/testing "simple forms"
    (t/is (not (insta/failure? (template "✳=abcd🔚 some text"))))
    (t/is (not (insta/failure? (template "text (with parens) and an expr ✳=(+ 3 4 5)🔚"))))
    (t/is (not (insta/failure? (template "text/text and an expr ✳=(+ 3 4 5)🔚"))))
    (t/is (not (insta/failure? (template "✳=(+ 3 4 5)🔚 some text"))))
    (t/is (not (insta/failure? (template "✳=(my.ns/fn  22)🔚 some text"))))
    (t/is (not (insta/failure? (template "✳(def something 2)🔚 some text"))))
    (t/is (not (insta/failure? (template "some text ✳(def something 2)🔚 some text"))))
    (t/is (not (insta/failure? (template "text ✳// more text //🔚 ✳(+ 3 4)🔚"))))

    (t/is (not (insta/failure?
                (template
                 "text ✳// more text
✳(+ 3 4)🔚
separate paragraphs
✳=(into [:div] (map inc (range 32 21 -1)))🔚
text
 //🔚 ")))))

  (t/testing "ambiguity"
    (doseq [f ["./pages/finite-schema-machines.html.fab"
               "./pages/fabricate.html.fab"
               "./README.md.fab"]]
      (let [c (slurp f)]
        (t/testing (str "in input file: " f)
          (t/is (not (insta/failure? (template c))))
          (t/is (= 1 (count (insta/parses template c)))
                "Each parser should parse only once and exactly once"))))))


(comment

  (template (slurp "./pages/fabricate.html.fab"))

  (template (slurp "./pages/finite-schema-machines.html.fab")
            :trace true)

  (template "text (with parens) and an expr ✳=(+ 3 4 5)🔚" :trace true)
  (template
                 "text ✳// more text
✳(+ 3 4)🔚
separate paragraphs
✳=(into [:div] (map inc (range 32 21 -1)))🔚
text
 //🔚 " :total true)



  (template (slurp "./README.md.fab") :trace true)

  )
