(ns site.fabricate.prototype.read-test
  (:require  [clojure.test :as t]
             [site.fabricate.prototype.page :refer [em link code blockquote]]
             [malli.core :as m]
             [hiccup.core :as hiccup]
             #_[respatialized.document :as doc]
             [site.fabricate.prototype.read :refer :all]))

(defn setup [f]
  (def parse-eval (comp eval-all parse))
  (require '[site.fabricate.prototype.page :refer [em link]])
  (f))

(t/use-fixtures :once setup)

(t/deftest regex
  (t/testing "regular expression"
    (t/is (= (re-matches parser-regex
                         (str (first delimiters) "=(+ 2 3)"
                              (last delimiters)))
             (re-matches (get-parser-regex (first delimiters)
                                           (last delimiters))
                         (str (first delimiters) "=(+ 2 3)"
                              (last delimiters)))
             [(str (first delimiters) "=(+ 2 3)" (last delimiters)) "" "=(+ 2 3)" ""]))

    (t/is (re-find parser-regex "✳=(+ 2 3)🔚"))
    (t/is (re-find parser-regex "some text, then code ✳=(+ 2 3)🔚"))))

(t/deftest evals
  (t/testing "expression evaluation"
    (t/is (= (eval-expr "=:foo") :foo)
          "Forms should be returned.")

    (t/is (= (eval-expr ":foo") nil)
          "Non-forms should be evaluated and not returned.")

    (t/is (= (eval-expr "=((+ 3 4)") :site.fabricate.prototype.read/parse-error)
          "Invalid exprs should return error values")
    (t/is (= (eval-expr "=(unknown-function 3 4)") :site.fabricate.prototype.read/parse-error)
          "Invalid exprs should return error values")))

(t/deftest parser

  (t/testing "parsed element model"
    (t/is
     (m/validate parsed-expr-model
                 {:src "✳=(+ 3 4)🔚"
                  :expr '(+ 3 4)
                  :err nil
                  :result 7}))
    (t/is
     (m/validate parsed-expr-model
                 {:src "✳(+ 3 4)🔚"
                  :expr '(do (+ 3 4) nil)
                  :err nil
                  :result nil}))
    (t/is
     (m/validate parsed-expr-model
                 {:src "✳((+ 3 4)🔚"
                  :expr nil
                  :err {:type clojure.lang.ExceptionInfo
                        :cause "Unexpected EOF while reading item 1 of list."
                        :phase nil
                        :message "Unexpected EOF while reading item 1 of list."}
                  :result nil})))

  (t/testing "expression parsing"
    (t/is (= ["text " {:expr '(+ 2 3)
                       :src "✳=(+ 2 3)🔚"
                       :err nil
                       :result nil}]
             (parse "text ✳=(+ 2 3)🔚")))

    (t/is (= [{:expr nil,
               :src "✳((+ 2 3)🔚",
               :err
               {:type clojure.lang.ExceptionInfo,
                :phase nil,
                :cause "Unexpected EOF while reading item 1 of list.",
                :message "Unexpected EOF while reading item 1 of list."},
               :result nil}]
             (parse "✳((+ 2 3)🔚")))
    (t/is (= [{:expr '(do (+ 2 3) nil)
               :src "✳(+ 2 3)🔚"
               :err nil
               :result nil}]
             (parse "✳(+ 2 3)🔚"))))

  (t/testing "evaluation of parsed expressions"
    (t/is (= 5 (eval-parsed-expr (first (parse "✳=(+ 2 3)🔚")) true)))
    (t/is (= {:expr '(+ 2 3), :src "✳=(+ 2 3)🔚", :err nil, :result 5}
             (eval-parsed-expr (first (parse "✳=(+ 2 3)🔚")) false)))
    (t/is (= nil
             (eval-parsed-expr {:expr '(do (def myvar 3) nil), :src "✳(def myvar 3)🔚", :err nil, :result nil}
                               true)))

    (t/is (= {:expr nil,
              :src "✳=((+ 2 3)🔚",
              :err {:type clojure.lang.ExceptionInfo,
                    :cause "Unexpected EOF while reading item 1 of list.",
                    :phase nil,
                    :message "Unexpected EOF while reading item 1 of list."},
              :result nil}
             (eval-parsed-expr (first (parse "✳=((+ 2 3)🔚")) false)
             (eval-parsed-expr (first (parse "✳=((+ 2 3)🔚")) true))))

  (t/testing "namespace retrieval"

    (t/is (= (symbol 'test-ns) (yank-ns (parse "✳(ns test-ns)🔚"))))

    (t/is (nil? (yank-ns (parse "✳=(+ 3 4)🔚"))))

    )

  (t/testing "metadata retrieval"
    (t/is (= '(def metadata {:title "Test" :namespace (ns site.fabricate.demo)})
             (-> "✳(def metadata {:title \"Test\" :namespace (ns site.fabricate.demo)})🔚"
                 parse
                 get-metadata)))

    (t/is (= nil
             (-> "✳(+ 3 4 5)🔚"
                 parse
                 get-metadata))))

  (t/testing "eval all"
    (let [parse-eval (comp eval-all parse)]
      (t/is (= (parse-eval "✳=:foo🔚 bar ✳=:baz🔚")
               [:foo " bar " :baz]))
      (t/is (= (parse-eval "some text") ["some text"])
            "Plaintext should be passed as-is")
      (t/is (= (parse-eval "✳=[1 2 3]🔚") [[1 2 3]]))
      (t/is (= (parse-eval "✳=[\"a\" \"b\"]🔚")  [["a" "b"]])
            "Escaped quotes in forms should be preserved.")
      (t/is (= [nil " baz " nil " foo " 3]
               (parse-eval "✳(ns test-form-ns)🔚 baz ✳(def var 3)🔚 foo ✳=var🔚"))
            "In-form defs should be evaluated successfully.")

      (t/is (= (parse-eval "✳=(site.fabricate.prototype.page/em 3)🔚")
               [[:em 3]])
            "Namespace scoping should be preserved")
      (t/is (= (parse-eval "✳=(em 3)🔚")
               [[:em 3]])
            "Namespace scoping should be preserved")

      (t/is (= [[:em "text"] ", with a comma following"]
               (parse-eval "✳=[:em \"text\"]🔚, with a comma following")
               ))

      (t/is (= (hiccup/html
                (apply conj [:div] (parse-eval "✳=[:em \"text\"]🔚, with a comma following")))
               "<div><em>text</em>, with a comma following</div>"))))

  (t/testing "eval with error messages"
    #_(t/is (m/validate
             (doc/subschema doc/html :doc/div)
             (form->hiccup {:expr nil,
                            :src "✳=((+ 2 3)🔚",
                            :err {:type clojure.lang.ExceptionInfo,
                                  :message "Unexpected EOF while reading item 1 of list."},
                            :result nil})))

    (t/is
     (= [:div 5]
        (eval-with-errors [:div {:expr '(+ 2 3), :src "✳=(+ 2 3)🔚", :err nil, :result nil}]))))

  (t/testing "string parse+eval"

    (t/is (= [:foo " bar " :baz]
             (parse-eval "✳=:foo🔚 bar ✳=:baz🔚")))
    (t/is (= ["some text"] (parse-eval "some text") )
          "Plaintext should be passed as-is")
    (t/is (= (parse-eval "some text" [:r-cell {:span "row"}])
             [:r-cell {:span "row"} "some text"])
          "Containing forms should be passed in correctly")
    (t/is (= (parse-eval "some text" [:r-grid {:columns 10}])
             [:r-grid {:columns 10} "some text"])
          "Containing forms should be passed in correctly")
    (t/is (= [[1 2 3]] (parse-eval "✳=[1 2 3]🔚") ))
    (t/is (= [["a" "b"]] (parse-eval "✳=[\"a\" \"b\"]🔚")  )
          "Escaped quotes in forms should be preserved.")
    (t/is (= [nil " foo " 3]  (eval-with-errors  (parse "✳(def var 3)🔚 foo ✳=var🔚")  'var-test-ns))
          "In-form defs should be evaluated successfully.")

    (t/is (= [[:em 3]]
             (parse-eval "✳=(site.fabricate.prototype.page/em 3)🔚"))
          "Namespace scoping should be preserved")

    (t/is (= [[:em 3]] (parse-eval "✳=(em 3)🔚"))
          "Namespace scoping should be preserved")

    (t/is (= [[:em "text"] ", with a comma following"]
             (parse-eval "✳=[:em \"text\"]🔚, with a comma following")))

    (t/is (= "<div><em>text</em>, with a comma following</div>"
             (hiccup/html (parse-eval "✳=[:em\"text\"]🔚, with a comma following" [:div]))))))
