(ns site.fabricate.read-test
  (:require  [clojure.test :as t]
             #_[site.fabricate.render :refer [em link code blockquote]]
             [malli.core :as m]
             [hiccup.core :as hiccup]
             #_[respatialized.document :as doc]
             [site.fabricate.parse :refer :all]))

(defn ns-refer [f]
  #_(require '[site.fabricate.render :refer [em]])
  (f))

(t/use-fixtures :once ns-refer)

(t/deftest evals
  (t/testing "expression evaluation"
    (t/is (= (eval-expr "=:foo") :foo)
          "Forms should be returned.")

    (t/is (= (eval-expr ":foo") nil)
          "Non-forms should be evaluated and not returned.")

    (t/is (= (eval-expr "=((+ 3 4)") :respatialized.parse/parse-error)
          "Invalid exprs should return error values")
    (t/is (= (eval-expr "=(unknown-function 3 4)") :respatialized.parse/parse-error)
          "Invalid exprs should return error values")))

(t/deftest parser

  (t/testing "parsed element model"
    (t/is
     (m/validate parsed-expr-model
               {:src "<%=(+ 3 4)%>"
                :expr '(+ 3 4)
                :err nil
                :result 7}))
    (t/is
     (m/validate parsed-expr-model
               {:src "<%(+ 3 4)%>"
                :expr '(do (+ 3 4) nil)
                :err nil
                :result nil}))
    (t/is
     (m/validate parsed-expr-model
               {:src "<%((+ 3 4)%>"
                :expr nil
                :err {:type clojure.lang.ExceptionInfo
                      :cause "Unexpected EOF while reading item 1 of list."
                      :phase nil
                      :message "Unexpected EOF while reading item 1 of list."}
                :result nil})))

  (t/testing "expression parsing"
    (t/is (= ["text " {:expr '(+ 2 3)
                       :src "<%=(+ 2 3)%>"
                       :err nil
                       :result nil}]
             (parse "text <%=(+ 2 3)%>")))

    (t/is (= [{:expr nil,
               :src "<%((+ 2 3)%>",
               :err
               {:type clojure.lang.ExceptionInfo,
                :phase nil,
                :cause "Unexpected EOF while reading item 1 of list.",
                :message "Unexpected EOF while reading item 1 of list."},
               :result nil}]
             (parse "<%((+ 2 3)%>")))
    (t/is (= [{:expr '(do (+ 2 3) nil)
               :src "<%(+ 2 3)%>"
               :err nil
               :result nil}]
             (parse "<%(+ 2 3)%>"))))

  (t/testing "evaluation of parsed expressions"
    (t/is (= 5 (eval-parsed-expr (first (parse "<%=(+ 2 3)%>")) true)))
    (t/is (= {:expr '(+ 2 3), :src "<%=(+ 2 3)%>", :err nil, :result 5}
             (eval-parsed-expr (first (parse "<%=(+ 2 3)%>")) false)))
    (t/is (= nil
             (eval-parsed-expr {:expr '(do (def myvar 3) nil), :src "<%(def myvar 3)%>", :err nil, :result nil}
                               true)))

    (t/is (= {:expr nil,
              :src "<%=((+ 2 3)%>",
              :err {:type clojure.lang.ExceptionInfo,
                    :cause "Unexpected EOF while reading item 1 of list.",
                    :phase nil,
                    :message "Unexpected EOF while reading item 1 of list."},
              :result nil}
             (eval-parsed-expr (first (parse "<%=((+ 2 3)%>")) false)
             (eval-parsed-expr (first (parse "<%=((+ 2 3)%>")) true))))

  (t/testing "namespace retrieval"

    (t/is (= (symbol 'test-ns) (yank-ns (parse "<%(ns test-ns)%>"))))

    (t/is (nil? (yank-ns (parse "<%=(+ 3 4)%>"))))

    )

  (t/testing "eval all"
    (let [parse-eval (comp eval-all parse)]
      (t/is (= (parse-eval "<%=:foo%> bar <%=:baz%>")
               [:foo " bar " :baz]))
      (t/is (= (parse-eval "some text") ["some text"])
            "Plaintext should be passed as-is")
      (t/is (= (parse-eval "<%=[1 2 3]%>") [[1 2 3]]))
      (t/is (= (parse-eval "<%=[\"a\" \"b\"]%>")  [["a" "b"]])
            "Escaped quotes in forms should be preserved.")
      (t/is (= [nil " baz " nil " foo " 3]
               (parse-eval "<%(ns test-form-ns)%> baz <%(def var 3)%> foo <%=var%>"))
            "In-form defs should be evaluated successfully.")

      (t/is (= (parse-eval "<%=(respatialized.render/em 3)%>")
               [[:em 3]])
            "Namespace scoping should be preserved")
      (t/is (= (parse-eval "<%=(em 3)%>")
               [[:em 3]])
            "Namespace scoping should be preserved")

      (t/is (= [[:em "text"] ", with a comma following"]
               (parse-eval "<%=[:em \"text\"]%>, with a comma following")
               ))

      (t/is (= (hiccup/html
                (apply conj [:div] (parse-eval "<%=[:em \"text\"]%>, with a comma following")))
               "<div><em>text</em>, with a comma following</div>"))))

  (t/testing "eval with error messages"
    #_(t/is (m/validate
           (doc/subschema doc/html :doc/div)
           (form->hiccup {:expr nil,
                          :src "<%=((+ 2 3)%>",
                          :err {:type clojure.lang.ExceptionInfo,
                                :message "Unexpected EOF while reading item 1 of list."},
                          :result nil})))

    (t/is
     (= [:div 5]
        (eval-with-errors [:div {:expr '(+ 2 3), :src "<%=(+ 2 3)%>", :err nil, :result nil}]))))

  (t/testing "string parse+eval"

    (t/is (= (parse-eval "<%=:foo%> bar <%=:baz%>")
             [:div :foo " bar " :baz]))
    (t/is (= (parse-eval "some text") [:div "some text"])
          "Plaintext should be passed as-is")
    (t/is (= (parse-eval "some text" [:r-cell {:span "row"}])
             [:r-cell {:span "row"} "some text"])
          "Containing forms should be passed in correctly")
    (t/is (= (parse-eval "some text" [:r-grid {:columns 10}])
             [:r-grid {:columns 10} "some text"])
          "Containing forms should be passed in correctly")
    (t/is (= (parse-eval "<%=[1 2 3]%>") [:div [1 2 3]]))
    (t/is (= (parse-eval "<%=[\"a\" \"b\"]%>")  [:div ["a" "b"]])
          "Escaped quotes in forms should be preserved.")
    (t/is (= (parse-eval "<%(def var 3)%> foo <%=var%>" [:div] "var-test-ns") [:div " foo " 3])
          "In-form defs should be evaluated successfully.")

    (t/is (= (parse-eval "<%=(respatialized.render/em 3)%>")
             [:div [:em 3]])
          "Namespace scoping should be preserved")
    (t/is (= (parse-eval "<%=(em 3)%>")
             [:div [:em 3]])
          "Namespace scoping should be preserved")

    (t/is (= (parse-eval "<%=(link \"here\" \"text\")%>" [:div] "link-ns" '[[respatialized.render :refer [link]]])
             [:div (link "here" "text")])
          "Links should parse + eval using external namespaces")

    (t/is (= (parse-eval "an inline <%=(link \"here\" \"link\")%>, followed by some <%=(em \"emphasis\").%>"
                         [:div]
                         "link-em-ns"
                         '[[respatialized.render :refer [link em]]])
             [:div
              "an inline "
              (link "here" "link")
              ", followed by some "
              (em "emphasis")])
          "All elements should end up in vector forms")

    (t/is (= (parse-eval "<%=[:em \"text\"]%>, with a comma following")
             [:div [:em "text"] ", with a comma following"]))

    (t/is (= (hiccup/html (parse-eval "<%=[:em\"text\"]%>, with a comma following"))
             "<div><em>text</em>, with a comma following</div>"))))
