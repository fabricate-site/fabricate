(ns site.fabricate.prototype.read.grammar-test
  (:require [site.fabricate.prototype.read.grammar  :refer :all]
            [instaparse.core :as insta]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [malli.generator :as mg]
            [clojure.java.io :as io]
            [clojure.test :as t]
            [clojure.test.check.clojure-test :refer [defspec]]
            ))

(t/deftest parser

  (t/testing "rules"
    (t/is (insta/failure?
           (template "some text with unbalanced iniital â³ and more txt"
                     :start :txt))
          "Unbalanced start characters should cause failure")
    (t/is (insta/failure?
           (template "some text with unbalanced iniital â³ and more txt"
                     :start :template))
          "Unbalanced start characters should cause failure")

    (t/testing " for extended forms"
      (t/is (not (insta/failure? (template "â³//[:div \n more text ]//ð" :start :extended-form))))
      (t/is (not (insta/failure? (template "â³//[\n â³(+ 3 4 5)ð ]//ð" :start :extended-form))))))

  (t/testing "simple forms"
    (t/is (not (insta/failure? (template "â³=abcdð some text"))))

    (t/is (let [r (template "text (with parens) and an expr â³=(+ 3 4 5)ð")]
            (and
             (= [:template [:txt "text (with parens) and an expr "] [:expr [:ctrl "="] "(+ 3 4 5)"]]
                r)
             (not (insta/failure? r)))))
    (t/is (not (insta/failure? (template "text (with parens) and an expr â³=(+ 3 4 5)ð and a trailing newline\n"))))
    (t/is (not (insta/failure? (template "text/text and an expr â³=(+ 3 4 5)ð")))
          "Grammar should recognize plaintext with common usage of special characters")
    (t/is (= [:template [:expr [:ctrl "="] "(+ 3 4 5)"] [:txt " some text"]]
             (template "â³=(+ 3 4 5)ð some text")))
    (t/is (= [:template [:expr [:ctrl "+"] "(+ 3 4 5)"] [:txt " some text"]]
             (template "â³+(+ 3 4 5)ð some text")))
    (t/is (= [:template [:expr [:ctrl "+="] "(+ 3 4 5)"] [:txt " some text"]]
             (template "â³+=(+ 3 4 5)ð some text")))
    (t/is (= [:template [:expr [:ctrl "="] "(my.ns/fn  22)"] [:txt " some text"]]
             (template "â³=(my.ns/fn  22)ð some text")))

    (t/is
     (= [:template [:expr "(def something 2)"] [:txt " some text"]]
        (template "â³(def something 2)ð some text")))

    (t/is  (= [:template [:txt "some text "] [:expr "(def something 2)"] [:txt " some text"]]
              (template "some text â³(def something 2)ð some text")))
    (t/is (not (insta/failure? (template "text â³//[\n more text ]//ð an expr â³(+ 3 4)ð"))))
    (t/is (not (insta/failure? (template "text â³//[\n more text â³//(\n (str 23) )//ð ]//ð an expr â³(+ 3 4)ð")))
          "Extended expressions should nest")

    (t/is (some
           #{":div"}
           (flatten (template "text â³//[:div\n more text â³//(\n (str 23) )//ð ]//ð an expr â³(+ 3 4)ð")))
          "Extended expressions should yield front matter")

    (t/is (insta/failure? (template "text â³//[\n more text â³//(\n (str 23) }//ð ]//ð an expr â³(+ 3 4)ð"))
          "Unbalanced extended forms should cause parse failures")
    (t/is (not (insta/failure?
                (template
                 "text â³//[
more text
â³(+ 3 4)ð
separate paragraphs
â³=(into [:div] (map inc (range 32 21 -1)))ð
text
 ]//ð "))))))

(t/deftest pages
  (t/testing "ambiguity"
    (doseq [f ["./pages/background/finite-schema-machines.html.fab"
               "./pages/fab.html.fab"
               "./pages/index.html.fab"
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
â³=[:h1 (:title metadata)]ð

â³=[:h4 \"form by art and labor\"]ð


Introducing fabricate, a Clojure library for making static websites, using Clojure.

â³//[

Multi-line form here
]//ð
")

  (template (slurp "./pages/index.html.fab"))

  (template
   (slurp "./pages/background/finite-schema-machines.html.fab")))

(def pathological-input-schema
    "Malli schema for problematic input cases"
    (m/schema
     [:orn
      [:unclosed-tag
       [:cat [:? :string] [:enum "â³" "â³=" "â³+" "â³+="]
        [:string]
        [:? :string]]]
      [:trailing-end
       [:cat :string [:= "ð"] [:? [:string]]]]]))

(def pathological-input-generator
    (gen/fmap #(apply str %) (mg/generator pathological-input-schema)))

(defspec pathological-input-detection 8000
  (prop/for-all
   [input pathological-input-generator]
   (insta/failure? (template input))))
