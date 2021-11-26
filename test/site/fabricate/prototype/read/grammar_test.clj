(ns site.fabricate.prototype.read.grammar-test
  (:require [site.fabricate.prototype.read.grammar  :refer :all]
            [instaparse.core :as insta]
            [clojure.java.io :as io]
            [clojure.test :as t]))

(t/deftest parser

  (t/testing "rules"
    (t/is (insta/failure?
           (template "some text with unbalanced iniital ✳ and more txt"
                     :start :txt))
          "Unbalanced start characters should cause failure")
    (t/is (insta/failure?
           (template "some text with unbalanced iniital ✳ and more txt"
                     :start :template))
          "Unbalanced start characters should cause failure")

    (t/testing " for extended forms"
      (t/is (not (insta/failure? (template "✳//[:div \n more text ]//🔚" :start :extended-form))))
      (t/is (not (insta/failure? (template "✳//[\n ✳(+ 3 4 5)🔚 ]//🔚" :start :extended-form))))))

  (t/testing "simple forms"
    (t/is (not (insta/failure? (template "✳=abcd🔚 some text"))))

    (t/is (let [r (template "text (with parens) and an expr ✳=(+ 3 4 5)🔚")]
            (and
             (= [:template [:txt "text (with parens) and an expr "] [:expr [:ctrl "="] "(+ 3 4 5)"]]
                r)
             (not (insta/failure? r)))))
    (t/is (not (insta/failure? (template "text (with parens) and an expr ✳=(+ 3 4 5)🔚 and a trailing newline\n"))))
    (t/is (not (insta/failure? (template "text/text and an expr ✳=(+ 3 4 5)🔚")))
          "Grammar should recognize plaintext with common usage of special characters")
    (t/is (= [:template [:expr [:ctrl "="] "(+ 3 4 5)"] [:txt " some text"]]
             (template "✳=(+ 3 4 5)🔚 some text")))
    (t/is (= [:template [:expr [:ctrl "+"] "(+ 3 4 5)"] [:txt " some text"]]
             (template "✳+(+ 3 4 5)🔚 some text")))
    (t/is (= [:template [:expr [:ctrl "+="] "(+ 3 4 5)"] [:txt " some text"]]
             (template "✳+=(+ 3 4 5)🔚 some text")))
    (t/is (= [:template [:expr [:ctrl "="] "(my.ns/fn  22)"] [:txt " some text"]]
             (template "✳=(my.ns/fn  22)🔚 some text")))

    (t/is
     (= [:template [:expr "(def something 2)"] [:txt " some text"]]
        (template "✳(def something 2)🔚 some text")))

    (t/is  (= [:template [:txt "some text "] [:expr "(def something 2)"] [:txt " some text"]]
              (template "some text ✳(def something 2)🔚 some text")))
    (t/is (not (insta/failure? (template "text ✳//[\n more text ]//🔚 an expr ✳(+ 3 4)🔚"))))
    (t/is (not (insta/failure? (template "text ✳//[\n more text ✳//(\n (str 23) )//🔚 ]//🔚 an expr ✳(+ 3 4)🔚")))
          "Extended expressions should nest")

    (t/is (some
           #{":div"}
           (flatten (template "text ✳//[:div\n more text ✳//(\n (str 23) )//🔚 ]//🔚 an expr ✳(+ 3 4)🔚")))
          "Extended expressions should yield front matter")

    (t/is (insta/failure? (template "text ✳//[\n more text ✳//(\n (str 23) }//🔚 ]//🔚 an expr ✳(+ 3 4)🔚"))
          "Unbalanced extended forms should cause parse failures")
    (t/is (not (insta/failure?
                (template
                 "text ✳//[
more text
✳(+ 3 4)🔚
separate paragraphs
✳=(into [:div] (map inc (range 32 21 -1)))🔚
text
 ]//🔚 "))))))

(t/deftest pages
  (t/testing "ambiguity"
    (doseq [f ["./pages/finite-schema-machines.html.fab"
               "./pages/fabricate.html.fab"
               "./README.md.fab"]]
      (let [c (slurp f)]
        (t/testing (str "in input file: " f)
          (t/is (not (insta/failure? (template c)))
                "Each template should successfully parse")
          (t/is (= 1 (count
                      (take 5 (insta/parses template c))))
                "Each template should parse only once and exactly once")))))

  ;; leveraging github for this is the simplest thing that works
  ;; a localized version using jgit is preferable, but modifying the working
  ;; tree while running tests seems fraught with pitfalls
  (t/testing "backwards compatibility"
    (doseq [p ["https://raw.githubusercontent.com/fabricate-site/fabricate/d0b44d79f78d081b7a2282ccde97e69cfa390ca5/pages/fabricate.html.fab"
               "https://raw.githubusercontent.com/fabricate-site/fabricate/d0b44d79f78d081b7a2282ccde97e69cfa390ca5/pages/finite-schema-machines.html.fab"
               "https://raw.githubusercontent.com/fabricate-site/fabricate/d0b44d79f78d081b7a2282ccde97e69cfa390ca5/pages/index.html.fab"]]
      (let [page-name (last (clojure.string/split p #"/"))]
        (t/testing (str " for page " page-name)
          (t/is (not (insta/failure? (template (slurp p))))))))))

(comment

  (let [f (slurp "./pages/finite-schema-machines.html.fab")]
    (crit/with-progress-reporting
      (crit/bench
       (template f))))

  (template "
✳=[:h1 (:title metadata)]🔚

✳=[:h4 \"form by art and labor\"]🔚


Introducing fabricate, a Clojure library for making static websites, using Clojure.

✳//[

Multi-line form here
]//🔚
")

  (template (slurp "./pages/fabricate.html.fab"))

  (template
   (slurp "./pages/finite-schema-machines.html.fab")))
