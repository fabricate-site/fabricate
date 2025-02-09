(ns site.fabricate.prototype.source.clojure-test
  (:require [clojure.test :as t]
            [clojure.java.io :as io]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.node :as node]
            [babashka.fs :as fs]
            [hiccup.page]
            [edamame.core :as e]
            [clojure.tools.reader :as reader]
            [matcher-combinators.test]
            [matcher-combinators.matchers :as m]
            [malli.core :as malli]
            [malli.error :as me]
            [site.fabricate.prototype.html :as html]
            [site.fabricate.prototype.source.clojure :as clj]
            [site.fabricate.api :as api]))



(t/deftest extract-functions
  (t/testing "clojure comments"
    (t/is
     (=
      (list
       "in the assemble step, should the Clojure code be treated as a \"block\"?"
       [:br {:class "clojure-newline"}])
      (#'clj/extract-comment-text
       ";; in the assemble step, should the Clojure code be treated as a \"block\"?\n")))
    (t/is (= '("a comment") (#'clj/extract-comment-text ";; a comment ")))))

(t/deftest hiccup
  (t/testing "paragraph merging"
    (t/is (= (list [:pre {:class "clojure-form"}
                    [:code {:class "language-clojure"}] "(+ 3 4)"]
                   [:p {:class "clojure-comment"} "Clojure example"])
             (clj/merge-paragraphs
              [:pre {:class "clojure-form"} [:code {:class "language-clojure"}]
               "(+ 3 4)"]
              {:clojure/comment      "Clojure example"
               :clojure.comment/text '("Clojure example")}))
          "Comment following code should start new paragraph")
    (t/is (= (list [:p {:class "clojure-comment"} "Clojure example"
                    [:br {:class "clojure-newline"}]])
             (clj/merge-paragraphs [:p {:class "clojure-comment"}
                                    "Clojure example"]
                                   {:clojure/newlines "\n"}))
          "Newlines should be appended to a paragraph")
    (t/is
     (= (list [:p {:class "clojure-comment"} "Clojure example"
               [:br {:class "clojure-newline"}] [:br {:class "clojure-newline"}]
               [:br {:class "clojure-newline"}]])
        (clj/merge-paragraphs [:p {:class "clojure-comment"} "Clojure example"]
                              {:clojure/newlines "\n\n\n"}))
     "Newlines should be appended to a paragraph")
    (t/is (= (list [:p {:class "clojure-comment"} "Clojure example"]
                   [:p {:class "clojure-comment"} "next paragraph"])
             (clj/merge-paragraphs [:p {:class "clojure-comment"}
                                    "Clojure example" [:br] [:br]]
                                   {:clojure/comment      "next paragraph"
                                    :clojure.comment/text '("next paragraph")}))
          "Paragraphs should be separated by multiple newlines and trimmed")
    (t/is (= (list [:p {:class "clojure-comment"} "comment example" " "
                    "with single linebreak"])
             (clj/merge-paragraphs
              [:p {:class "clojure-comment"} "comment example"
               [:br {:class "clojure-newline"}]]
              {:clojure/comment      "with single linebreak"
               :clojure.comment/text '("with single linebreak")}))
          "Single linebreaks should be be added to paragraph with whitespace")
    (t/is (= (apply list
                    [:p {:class "clojure-comment"} "Clojure example"]
                    (#'clj/code-block {:clojure/result "(+ 3 4)"}))
             (clj/merge-paragraphs [:p {:class "clojure-comment"}
                                    "Clojure example"]
                                   {:clojure/result "(+ 3 4)"}))
          "Code following comment should begin new element")
    (t/is (= (list [:p "comment example"])
             (clj/merge-paragraphs [:p "comment example"]
                                   {:clojure/uneval "#_nil"}))
          "Uneval forms should be skipped")
    (t/is (= (list {:class "attribute-example"}
                   [:p {:class "clojure-comment"} "comment"])
             (clj/merge-paragraphs {:class "attribute-example"}
                                   {:clojure/comment      "comment"
                                    :clojure.comment/text '("comment")}))
          "Attribute maps should be preserved before comments")
    (t/is (= (apply list
                    {:class "attribute-example"}
                    (#'clj/code-block {:clojure/result "string"}))
             (clj/merge-paragraphs {:class "attribute-example"}
                                   {:clojure/result "string"}))
          "Attribute maps should be preserved before results")
    (t/is (= (list [:pre [:code {:class "language-clojure"} nil]])
             (clj/merge-paragraphs [:pre
                                    [:code {:class "language-clojure"} nil]]
                                   {:clojure/newlines "\n\n\n"}))
          "Newlines following a code block should be discarded"))
  (t/testing "postprocessing"
    (let
      [hide-code-str "^:kindly/hide-code '(hidden-form \"example\")"
       code-block-results (-> hide-code-str
                              parser/parse-string
                              clj/normalize-node
                              clj/eval-form
                              (#'clj/code-block))
       hide-both-results
       (->
         "^{:kindly/hide-code true :kindly/hide-result true} (def example-hidden 1 )"
         parser/parse-string
         clj/normalize-node
         clj/eval-form
         (#'clj/code-block))]
      (t/is (= "clojure-result" (get-in (first code-block-results) [1 :class])))
      (t/is
       (= '() hide-both-results)
       "When both code and result are hidden, no Hiccup should be generated.")
      (t/is
       (=
        [:main {:data-clojure-namespace 'example-ns}
         [:p {:class "clojure-comment"} "example ns"] [:h1 "Example ns"]]
        (->
          "^{:kindly/hide-code true :kindly/hide-result true} (ns example-ns)
;; example ns
^{:kindly/kind :kind/hiccup} [:h1 \"Example ns\"]"
          clj/string->forms
          clj/eval-forms
          clj/forms->hiccup))
       "Hidden namespaces should not be included in final hiccup"))
    #_(t/is (malli/validate html/html
                            (-> "test-resources/site/fabricate/example.clj"
                                clj/file->forms
                                clj/eval-forms
                                clj/forms->hiccup)))
    (let [final-entry  (api/build {:site.fabricate.source/location
                                   "test-resources/site/fabricate/example.clj"
                                   :site.fabricate.source/format :clojure/v0
                                   :site.fabricate.document/format :hiccup/html}
                                  {})
          final-hiccup (:site.fabricate.document/data final-entry)]
      (t/is (vector? final-hiccup))
      (t/is
       (string? (hiccup.page/html5 final-hiccup))
       "Hiccup produced from Clojure namespace should be rendered without errors"))))

(t/deftest node-functions
  (t/testing "metadata normalization"
    (t/is (= {:kindly/hide-code true}
             (#'clj/meta-node->metadata-map
              (parser/parse-string "^:kindly/hide-code [1 2 3]")))
          "keyword metadata should be normalized into a map")
    (t/is (= {:type 'Double}
             (#'clj/meta-node->metadata-map (parser/parse-string "^Double a")))
          "keyword metadata should be normalized into a map")
    (t/is (= {:kindly/kind :kind/code}
             (-> "^{:kindly/kind :kind/code} '(+ 3 4 5)"
                 parser/parse-string
                 clj/normalize-node
                 :clojure/metadata))
          "metadata should be placed in the normalized form map")
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
          (let [forms      (try (:clojure/forms (clj/file->forms (fs/file
                                                                  src-file)))
                                (catch Exception e
                                  (let [err (Throwable->map e)]
                                    [{:type  :invalid
                                      :error err
                                      :file  (str src-file)}])))
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
  (require '[hiccup.page])
  (->> (api/build {:site.fabricate.source/location
                   "test-resources/site/fabricate/example.clj"
                   :site.fabricate.source/format :clojure/v0
                   :site.fabricate.document/format :hiccup/html}
                  {})
       :site.fabricate.document/data
       hiccup.page/html5
       (spit "test-resources/html/site.fabricate.example.html"))
  (require '[edamame.core :as e])
  (require '[matcher-combinators.standalone :as match])
  (match/match (m/any-of {:a int?} {:x string?}) {:a 1})
  (match/match (m/any-of {:a int?} {:x string?}) {:x "test"}))
