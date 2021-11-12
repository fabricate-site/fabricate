(ns site.fabricate.prototype.read.grammar-test
  (:require [site.fabricate.prototype.read.grammar  :refer :all]
            [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.test :as t]))

(t/deftest parser

  (t/testing "rules"
    (t/is (insta/failure?
           (template "some text with unbalanced iniital ✳ and more txt"
                     :rule :txt))))

  (t/testing "simple forms"
    (t/is (not (insta/failure? (template "✳=abcd🔚 some text"))))
    (t/is (not (insta/failure? (template "text (with parens) and an expr ✳=(+ 3 4 5)🔚"))))
    (t/is (not (insta/failure? (template "text (with parens) and an expr ✳=(+ 3 4 5)🔚 and a trailing newline\n"))))
    (t/is (not (insta/failure? (template "text/text and an expr ✳=(+ 3 4 5)🔚")))
          "Grammar should recognize plaintext with common usage of special characters")
    (t/is (not (insta/failure? (template "✳=(+ 3 4 5)🔚 some text"))))
    (t/is (not (insta/failure? (template "✳=(my.ns/fn  22)🔚 some text"))))
    (t/is (not (insta/failure? (template "✳(def something 2)🔚 some text"))))
    (t/is (not (insta/failure? (template "some text ✳(def something 2)🔚 some text"))))
    #_(t/is (not (insta/failure? (template "text ✳//\nmore text //🔚 ✳(+ 3 4)🔚"))))

    #_(t/is (not (insta/failure?
                  (template
                   "text ✳// more text
✳(+ 3 4)🔚
separate paragraphs
✳=(into [:div] (map inc (range 32 21 -1)))🔚
text
 //🔚 "))))))

(t/deftest pages
  (t/testing "ambiguity"
    (doseq [f ["./pages/finite-schema-machines.html.fab"
               "./pages/fabricate.html.fab"
               "./README.md.fab"]]
      (let [c (slurp f)]
        (t/testing (str "in input file: " f)
          (t/is (not (insta/failure? (template c)))
                "Each template should successfully parse")
          #_(t/is (= 1 (count
                        (take 2 (insta/parses template c))))
                  "Each template should parse only once and exactly once"))))))


(comment

  (require '[criterium.core :as crit])

  ()

  (template "some final text - true/false")

  (template "
✳=[:h1 (:title metadata)]🔚

✳=[:h4 \"form by art and labor\"]🔚


Introducing fabricate, a Clojure library for making static websites, using Clojure.

")


  (template (slurp "./pages/fabricate.html.fab")
            :total true)

  (template (slurp "./pages/finite-schema-machines.html.fab"))

  (doseq [p (insta/parses
             template
             (slurp "./pages/finite-schema-machines.html.fab"))]
    (println p))

  (count (take 50 (insta/parses template
                                (slurp "./pages/finite-schema-machines.html.fab"))))

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
