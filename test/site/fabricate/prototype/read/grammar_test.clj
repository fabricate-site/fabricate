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
      (t/is (not (insta/failure? (template "✳//[\n more text ]//🔚" :start :extended-form))))
      (t/is (not (insta/failure? (template "✳//[\n ✳(+ 3 4 5)🔚 ]//🔚" :start :extended-form))))))

  (t/testing "simple forms"
    (t/is (not (insta/failure? (template "✳=abcd🔚 some text"))))

    (t/is (let [r (template "text (with parens) and an expr ✳=(+ 3 4 5)🔚")]
            (and
             (= [:template [:txt "text (with parens) and an expr "] [:expr "=" "(+ 3 4 5)"]]
                r)
             (not (insta/failure? r)))))
    (t/is (not (insta/failure? (template "text (with parens) and an expr ✳=(+ 3 4 5)🔚 and a trailing newline\n"))))
    (t/is (not (insta/failure? (template "text/text and an expr ✳=(+ 3 4 5)🔚")))
          "Grammar should recognize plaintext with common usage of special characters")
    (t/is (= [:template [:expr "=" "(+ 3 4 5)"] [:txt " some text"]]
             (template "✳=(+ 3 4 5)🔚 some text")))
    (t/is (= [:template [:expr "=" "(my.ns/fn  22)"] [:txt " some text"]]
             (template "✳=(my.ns/fn  22)🔚 some text")))

    (t/is
     (= [:template [:expr "(def something 2)"] [:txt " some text"]]
        (template "✳(def something 2)🔚 some text")))

    (t/is  (= [:template [:txt "some text "] [:expr "(def something 2)"] [:txt " some text"]]
              (template "some text ✳(def something 2)🔚 some text")))
    (t/is (not (insta/failure? (template "text ✳//[\n more text ]//🔚 an expr ✳(+ 3 4)🔚"))))
    (t/is (not (insta/failure? (template "text ✳//[\n more text ✳//(\n (str 23) )//🔚 ]//🔚 an expr ✳(+ 3 4)🔚"))))
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

  (require '[criterium.core :as crit])

  (crit/with-progress-reporting
    ;;           Execution time mean : 4.633221 µs
    ;;  Execution time std-deviation : 1.402354 µs
    ;; Execution time lower quantile : 3.436020 µs ( 2.5%)
    ;; Execution time upper quantile : 6.546986 µs (97.5%)
    ;;                 Overhead used : 8.073388 ns
    (crit/quick-bench
     (re-seq #"(\A[^✳🔚]++\Z)|([\S\s]*?(?=(?:✳|/{2}?🔚)|\Z))"
             "text (with parens) and an expr ✳=(+ 3 4 5)🔚 and")))

  (crit/with-progress-reporting
    ;;               Execution time mean : 2.486980 µs
    ;;  Execution time std-deviation : 47.043546 ns
    ;; Execution time lower quantile : 2.437766 µs ( 2.5%)
    ;; Execution time upper quantile : 2.541947 µs (97.5%)
    ;;                 Overhead used : 8.073388 ns
    (crit/quick-bench
     (re-seq #"\A[\S\s]*?(?=\Z|(?:✳|/{2}?🔚))"
             "text (with parens) and an expr ✳=(+ 3 4 5)🔚 and")))

  (crit/with-progress-reporting
    ;;               Execution time mean : 3.103925 µs
    ;;  Execution time std-deviation : 931.925567 ns
    ;; Execution time lower quantile : 2.395667 µs ( 2.5%)
    ;; Execution time upper quantile : 4.609288 µs (97.5%)
    ;;                 Overhead used : 8.073388 ns
    (crit/quick-bench
     (re-seq #"\A[\S\s]*?(?=(?:✳|/{2}?🔚)|\Z)"
             "text (with parens) and an expr ✳=(+ 3 4 5)🔚 and")))

  (crit/with-progress-reporting
    ;;               Execution time mean : 3.103925 µs
    ;;  Execution time std-deviation : 931.925567 ns
    ;; Execution time lower quantile : 2.395667 µs ( 2.5%)
    ;; Execution time upper quantile : 4.609288 µs (97.5%)
    ;;                 Overhead used : 8.073388 ns
    (crit/quick-bench
     (re-seq #"(\A[^✳🔚]*+)|(\A[\S\s]*?(?=\Z|(?:✳|/{2}?🔚)))"
             "text (with parens) and an expr ✳=(+ 3 4 5)🔚 and")))

  (re-seq #"([^✳🔚]*+)|(\A[\S\s]*?(?=\Z|(?:✳|/{2}?🔚)))"
          "text (with parens) and an expr ✳=(+ 3 4 5)🔚 and")

  (crit/with-progress-reporting
    (crit/quick-bench
     (template (slurp "./README.md.fab"))))

  (template "
✳=[:h1 (:title metadata)]🔚

✳=[:h4 \"form by art and labor\"]🔚


Introducing fabricate, a Clojure library for making static websites, using Clojure.

")

  (template (slurp "./pages/fabricate.html.fab"))

  (time (template
         (slurp "./pages/finite-schema-machines.html.fab")))

  (doseq [p (insta/parses
             template
             (slurp "./pages/finite-schema-machines.html.fab"))]
    (println p))

  (count (take 50 (insta/parses template
                                (slurp "./pages/finite-schema-machines.html.fab"))))

  (template (slurp "./README.md.fab") :trace true)

  (count (insta/parses template (slurp "./pages/fabricate.html.fab")))

  (time
   (insta/parse template (slurp "./pages/fabricate.html.fab")))

  (insta/parses template "some text ✳(def something 2)🔚 some text"
                :rule :txt
                :partial true)

  (template "✳//[\n more text ]//🔚" :start :extended-form
            :trace true)



  )
