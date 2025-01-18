(ns site.fabricate.prototype.source.clojure-test
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.node :as node]
            [babashka.fs :as fs]
            [edamame.core :as e]
            [clojure.tools.reader :as reader]
            [matcher-combinators.test]
            [matcher-combinators.matchers :as m]
            [malli.core :as malli]
            [malli.error :as me]
            [site.fabricate.prototype.source.clojure :as clj]))



(t/deftest extract-functions
  (t/testing "clojure comments"
    (t/is
     (=
      "in the assemble step, should the Clojure code be treated as a \"block\"?\n"
      (#'clj/extract-comment-text
       ";; in the assemble step, should the Clojure code be treated as a \"block\"?\n")))
    (t/is (= "a comment " (#'clj/extract-comment-text ";; a comment ")))))

(t/deftest hiccup
  (t/testing "paragraph merging"
    (t/is (= (list [:pre [:code {:class "language-clojure"}] "(+ 3 4)"]
                   [:p "Clojure example"])
             (clj/merge-paragraphs [:pre [:code {:class "language-clojure"}]
                                    "(+ 3 4)"]
                                   {:clojure/comment "Clojure example"}))
          "Comment following code should start new paragraph")
    (t/is (= (list [:p "Clojure example" [:br]])
             (clj/merge-paragraphs [:p "Clojure example"]
                                   {:clojure/newlines "\n"}))
          "Newlines should be appended to a paragraph")
    (t/is (= (list [:p "Clojure example" [:br] [:br] [:br]])
             (clj/merge-paragraphs [:p "Clojure example"]
                                   {:clojure/newlines "\n\n\n"}))
          "Newlines should be appended to a paragraph")
    (t/is (= (list [:p "Clojure example"] [:p "next paragraph"])
             (clj/merge-paragraphs [:p "Clojure example" [:br] [:br]]
                                   {:clojure/comment "next paragraph"}))
          "Paragraphs should be separated by multiple newlines and trimmed")
    (t/is (= (list [:p "comment example" " " "with single linebreak"])
             (clj/merge-paragraphs [:p "comment example" [:br]]
                                   {:clojure/comment "with single linebreak"}))
          "Single linebreaks should be be added to paragraph with whitespace")
    (t/is (= (list [:p "Clojure example"]
                   [:pre [:code {:class "language-clojure"} "(+ 3 4)"]])
             (clj/merge-paragraphs [:p "Clojure example"]
                                   {:clojure/result "(+ 3 4)"}))
          "Code following comment should begin new element")
    (t/is (= (list [:p "comment example"])
             (clj/merge-paragraphs [:p "comment example"]
                                   {:clojure/uneval "#_nil"}))
          "Uneval forms should be skipped")))

(t/deftest node-functions
  (t/testing "metadata normalization"
    (t/is (= {:kindly/hide-code true}
             (#'clj/meta-node->metadata-map
              (parser/parse-string "^:kindly/hide-code [1 2 3]")))
          "keyword metadata should be normalized into a map")
    (t/is (= {:type 'Double}
             (#'clj/meta-node->metadata-map (parser/parse-string "^Double a")))
          "keyword metadata should be normalized into a map")
    (t/is
     (thrown? clojure.lang.ExceptionInfo
              (#'clj/meta-node->metadata-map (node/coerce [1 2 3 4])))
     "Nil/non-metadata nodes should throw an error when conversion is attempted")))

(t/deftest parsing
  (let [valid-form-map? (malli/validator clj/form-map-schema)
        form-explainer  (malli/explainer clj/form-map-schema)]
    (t/testing "parsing file into sequence of forms with rewrite-clj"
      (doseq [src-file (fs/glob "." "**.clj")]
        (t/testing (str "\n" src-file)
          (let [forms      (:clojure/forms (clj/file->forms (fs/file src-file)))
                all-valid? (every? valid-form-map? forms)]
            (when-not all-valid?
              (doseq [invalid-form (filter #(not (valid-form-map? %)) forms)]
                (println (dissoc (form-explainer invalid-form) :schema))
                (println (me/humanize (form-explainer invalid-form)))))
            (t/is all-valid? "Each parsed form should be valid.")))))))

(defn correct-example?
  "Is the example correct?"
  [{clj-form    :clojure/form
    clj-result  :clojure/result
    clj-error   :clojure/error
    clj-comment :clojure/comment
    :as         evaluated-form}]
  (or (and (some? clj-form) (nil? clj-error))
      (and (some? clj-error) (= "testing" (get-in clj-error [:context])))
      (string? clj-comment)))

(t/deftest evaluation
  (t/testing "parsing and evaluation of the example file"
    (let [evaluated-results (-> "test-resources/site/fabricate/example.clj"
                                clj/file->forms
                                clj/eval-forms)]
      (t/is (map? evaluated-results))
      (t/testing "result forms"
        (doseq [{clj-form :clojure/form :as result-form} (:clojure/forms
                                                          evaluated-results)]
          (t/is (match? (m/any-of {:clojure/form      any?
                                   :clojure/result    any?
                                   :clojure/namespace 'site.fabricate.example}
                                  {:clojure/comment   string?
                                   :clojure/namespace 'site.fabricate.example}
                                  {:clojure/newlines  string?
                                   :clojure/namespace 'site.fabricate.example}
                                  {:clojure/whitespace string?
                                   :clojure/namespace  'site.fabricate.example}
                                  {:clojure/error {:data {:severity :trivial
                                                          :context "testing"}}})
                        result-form))))))
  ;; eventually a test matrix for large files could be useful as a
  ;; regression test failsafe - not necessary for now.
  #_(t/testing "large namespace"))



(comment
  (require '[edamame.core :as e])
  (require '[matcher-combinators.standalone :as match])
  (match/match (m/any-of {:a int?} {:x string?}) {:a 1})
  (match/match (m/any-of {:a int?} {:x string?}) {:x "test"}))
