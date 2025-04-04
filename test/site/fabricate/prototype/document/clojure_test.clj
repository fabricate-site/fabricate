(ns site.fabricate.prototype.document.clojure-test
  (:require [site.fabricate.prototype.document.clojure :as clj]
            [site.fabricate.adorn :as adorn]
            [babashka.fs :as fs]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.node :as node]
            [malli.core :as malli]
            [malli.error :as me]
            [matcher-combinators.test]
            [matcher-combinators.matchers :as m]
            [clojure.test :as t]))

(def example-file "test-resources/site/fabricate/example.clj")


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

(t/deftest clj-read-eval
  (t/testing "source handling"
    (t/testing "clojure comments"
      (t/is
       (=
        (list
         "in the assemble step, should the Clojure code be treated as a \"block\"?"
         [:br {:class "clojure-newline"}])
        (#'clj/extract-comment-text
         ";; in the assemble step, should the Clojure code be treated as a \"block\"?\n")))
      (t/is (= '("a comment") (#'clj/extract-comment-text ";; a comment "))))
    (t/testing "metadata normalization"
      (t/is (= {:kindly/hide-code true}
               (#'clj/meta-node->metadata-map
                (parser/parse-string "^:kindly/hide-code [1 2 3]")))
            "keyword metadata should be normalized into a map")
      (t/is (= {:type 'Double}
               (#'clj/meta-node->metadata-map
                (parser/parse-string "^Double a")))
            "keyword metadata should be normalized into a map")
      (t/is (= {:kindly/kind :kind/code}
               (-> "^{:kindly/kind :kind/code} '(+ 3 4 5)"
                   parser/parse-string
                   clj/normalize-node
                   :meta))
            "metadata should be placed in the normalized node")
      (t/is (= {:kindly/kind :kind/code}
               (-> "^{:kindly/kind :kind/code} '(+ 3 4 5)"
                   parser/parse-string
                   clj/node->form
                   :clojure/metadata))
            "metadata should be placed in the normalized node")
      (t/is (= (node/coerce [1 2 3])
               (-> "^{:type :test} [1 2 3]"
                   parser/parse-string
                   clj/node->form
                   :clojure/node
                   (dissoc :meta)))
            "base node should be placed in form map as :clojure/node")
      (t/is
       (thrown? clojure.lang.ExceptionInfo
                (#'clj/meta-node->metadata-map (node/coerce [1 2 3 4])))
       "Nil/non-metadata nodes should throw an error when conversion is attempted")))
  (t/testing "individual forms"
    (= [1 2 3]
       (let [meta-example "^{:type :test} [1 2 3]"]
         (-> meta-example
             clj/read-forms
             :clojure/forms
             first
             clj/eval-form
             :clojure/result))))
  (t/testing "example file"
    (let [evaluated-results (-> example-file
                                clj/read-forms
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
  (t/testing "fabricate source code"
    (let [valid-form-map? (malli/validator clj/form-schema)
          form-explainer  (malli/explainer clj/form-schema)]
      (t/testing "parsing file into sequence of forms with rewrite-clj"
        (doseq [src-file (fs/glob "." "**.clj")]
          (t/testing (str "\n" src-file)
            (let [forms      (try (:clojure/forms (clj/read-forms (fs/file
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
              (t/is all-valid? "Each parsed form should be valid."))))))))

(t/deftest hiccup
  (t/testing "individual forms"
    (t/testing "paragraph merging"
      (t/is (= (list [:pre {:class "clojure-form"}
                      [:code {:class "language-clojure"}] "(+ 3 4)"]
                     [:p {:class "clojure-comment"} "Clojure example"])
               (#'clj/merge-paragraphs
                [:pre {:class "clojure-form"}
                 [:code {:class "language-clojure"}] "(+ 3 4)"]
                {:clojure/comment      "Clojure example"
                 :clojure.comment/text '("Clojure example")}))
            "Comment following code should start new paragraph")
      (t/is (= (list [:p {:class "clojure-comment"} "Clojure example"
                      [:br {:class "clojure-newline"}]])
               (#'clj/merge-paragraphs
                [:p {:class "clojure-comment"} "Clojure example"]
                {:clojure/newlines "\n"}))
            "Newlines should be appended to a paragraph")
      (t/is (= (list [:p {:class "clojure-comment"} "Clojure example"
                      [:br {:class "clojure-newline"}]
                      [:br {:class "clojure-newline"}]
                      [:br {:class "clojure-newline"}]])
               (#'clj/merge-paragraphs
                [:p {:class "clojure-comment"} "Clojure example"]
                {:clojure/newlines "\n\n\n"}))
            "Newlines should be appended to a paragraph")
      (t/is (= (list [:p {:class "clojure-comment"} "Clojure example"]
                     [:p {:class "clojure-comment"} "next paragraph"])
               (#'clj/merge-paragraphs
                [:p {:class "clojure-comment"} "Clojure example" [:br] [:br]]
                {:clojure/comment      "next paragraph"
                 :clojure.comment/text '("next paragraph")}))
            "Paragraphs should be separated by multiple newlines and trimmed")
      (t/is (= (list [:p {:class "clojure-comment"} "comment example" " "
                      "with single linebreak"])
               (#'clj/merge-paragraphs
                [:p {:class "clojure-comment"} "comment example"
                 [:br {:class "clojure-newline"}]]
                {:clojure/comment      "with single linebreak"
                 :clojure.comment/text '("with single linebreak")}))
            "Single linebreaks should be be added to paragraph with whitespace")
      (t/is (= (apply list
                      [:p {:class "clojure-comment"} "Clojure example"]
                      (#'clj/code-block {:clojure/result "(+ 3 4)"}))
               (#'clj/merge-paragraphs
                [:p {:class "clojure-comment"} "Clojure example"]
                {:clojure/result "(+ 3 4)"}))
            "Code following comment should begin new element")
      (t/is (= (list [:p "comment example"])
               (#'clj/merge-paragraphs
                [:p "comment example"]
                {:clojure/uneval "#_nil"}))
            "Uneval forms should be skipped")
      (t/is (= (list {:class "attribute-example"}
                     [:p {:class "clojure-comment"} "comment"])
               (#'clj/merge-paragraphs
                {:class "attribute-example"}
                {:clojure/comment      "comment"
                 :clojure.comment/text '("comment")}))
            "Attribute maps should be preserved before comments")
      (t/is (= (apply list
                      {:class "attribute-example"}
                      (#'clj/code-block {:clojure/result "string"}))
               (#'clj/merge-paragraphs
                {:class "attribute-example"}
                {:clojure/result "string"}))
            "Attribute maps should be preserved before results")
      (t/is (= (list [:pre [:code {:class "language-clojure"} nil]])
               (#'clj/merge-paragraphs
                [:pre [:code {:class "language-clojure"} nil]]
                {:clojure/newlines "\n\n\n"}))
            "Newlines following a code block should be discarded"))
    (t/testing "postprocessing"
      (let
        [hide-code-str "^:kindly/hide-code '(hidden-form \"example\")"
         code-block-results (-> hide-code-str
                                parser/parse-string
                                clj/node->form
                                clj/eval-form
                                (#'clj/code-block))
         hide-both-results
         (->
           "^{:kindly/hide-code true :kindly/hide-result true} (def example-hidden 1 )"
           parser/parse-string
           clj/node->form
           clj/eval-form
           (#'clj/code-block))]
        (t/is (= "clojure-result"
                 (get-in (first code-block-results) [1 :class])))
        (t/is
         (= '() hide-both-results)
         "When both code and result are hidden, no Hiccup should be generated.")
        (t/is
         (=
          [:article {:data-clojure-namespace 'example-ns}
           [:p {:class "clojure-comment"} "example ns"] [:h1 "Example ns"]]
          (->
            "^{:kindly/hide-code true :kindly/hide-result true} (ns example-ns)
;; example ns
^{:kindly/kind :kind/hiccup} [:h1 \"Example ns\"]"
            clj/read-forms
            clj/eval-forms
            clj/forms->hiccup))
         "Hidden namespaces should not be included in final hiccup"))
      #_(t/is (malli/validate html/html
                              (-> "test-resources/site/fabricate/example.clj"
                                  clj/file->forms
                                  clj/eval-forms
                                  clj/forms->hiccup)))
      #_(let [final-entry  (api/build
                            {:site.fabricate.source/location
                             "test-resources/site/fabricate/example.clj"
                             :site.fabricate.source/format :clojure/v0
                             :site.fabricate.document/format :hiccup/html}
                            {})
              final-hiccup (:site.fabricate.document/data final-entry)]
          (t/is (vector? final-hiccup))
          (t/is
           (string? (hiccup.page/html5 final-hiccup))
           "Hiccup produced from Clojure namespace should be rendered without errors")))
    (let [src-str "^{:type :test} [1 2 3]"]
      (t/is (= (list [:pre {:class "clojure-form"}
                      [:code {:class "language-clojure"}
                       (first (adorn/clj->hiccup "[1 2 3]"))]]
                     [:pre {:class "clojure-result"}
                      [:code {:class "language-clojure"}
                       (adorn/clj->hiccup [1 2 3])]])
               (#'clj/code-block
                {:clojure/source   "^{:type :test} [1 2 3]"
                 :clojure/result   [1 2 3]
                 :clojure/node     (clj/normalize-node
                                    (parser/parse-string
                                     "^{:type :test} [1 2 3]"))
                 :clojure/metadata {:type :test}}))
            "Metadata on forms' source code should be hidden by default")
      (t/is
       (= (list
           [:pre {:class "clojure-form"}
            [:code {:class "language-clojure"}
             (first (adorn/clj->hiccup
                     "^{:type :test :kindly/hide-metadata false} [1 2 3]"))]]
           [:pre {:class "clojure-result"}
            [:code {:class "language-clojure"} (adorn/clj->hiccup [1 2 3])]])
          (#'clj/code-block
           {:clojure/source "^{:type :test :kindly/hide-metadata false} [1 2 3]"
            :clojure/result [1 2 3]
            :clojure/metadata {:type :test :kindly/hide-metadata false}
            :clojure/node
            (clj/normalize-node
             (parser/parse-string
              "^{:type :test :kindly/hide-metadata false} [1 2 3]"))}))
       "Metadata on forms' source code should be displayed when option is passed")))
  (t/testing "example file"))

(t/deftest kindly
  (t/testing "individual forms"
    (t/is (= ["[1 2 3]"]
             (-> "^{:type :test} [1 2 3]"
                 clj/read-forms
                 :clojure/forms
                 first
                 clj/eval-form
                 clj/form->kind
                 first))
          "Metadata on forms' source code should be hidden by default")
    (t/is
     (= ["^{:type :test :kindly/hide-metadata false} [1 2 3]"]
        (-> "^{:type :test :kindly/hide-metadata false} [1 2 3]"
            clj/read-forms
            :clojure/forms
            first
            clj/eval-form
            clj/form->kind
            first))
     "Metadata on forms' source code should be displayed when option is passed"))
  (t/testing "example file"
    (let [result-fragment (-> example-file
                              clj/read-forms
                              clj/eval-forms
                              clj/forms->fragment)]
      (t/is (= :kind/fragment
               (-> result-fragment
                   meta
                   :kindly/kind))
            "Document should be processed into kindly fragment"))))
