(ns site.fabricate.prototype.read-test
  (:require  [clojure.test :as t]
             [site.fabricate.prototype.page :refer [em link code blockquote]]
             [malli.core :as m]
             [malli.instrument :as mi]
             [hiccup.core :as hiccup]
             [clojure.java.io :as io]
             [site.fabricate.prototype.read.grammar :refer [template]]
             [site.fabricate.prototype.read :refer :all]))

(defn setup [f]
  (def parse-eval (comp eval-all parse))
  (require '[site.fabricate.prototype.page :refer [em link]])
  (mi/collect!)
  (mi/instrument!)
  (f)
  (mi/unstrument!))

(t/use-fixtures :once setup)

(t/deftest file-utils
  (t/testing "Filename utilities"
    (t/is (= {:site.fabricate.file/filename "README"
              :site.fabricate.file/output-extension "md"
              :site.fabricate.file/template-suffix ".fab"}
             (get-file-metadata "./README.md.fab")))

    (t/is (= {:site.fabricate.file/filename "pages/reference/namespaces/site.fabricate.prototype.write"
              :site.fabricate.file/output-extension "html"
              :site.fabricate.file/template-suffix ".fab"}
             (get-file-metadata
              "./pages/reference/namespaces/site.fabricate.prototype.write.html.fab")))

    (t/is (= {:site.fabricate.file/filename "some.dir/docs/README"
              :site.fabricate.file/output-extension "md"
              :site.fabricate.file/template-suffix ".fab"}
             (get-file-metadata "./some.dir/docs/README.md.fab")))

    (t/is (= {:site.fabricate.file/filename "content/test"
              :site.fabricate.file/output-extension "md"
              :site.fabricate.file/template-suffix ".fab"}
             (get-file-metadata "./content/test.md.fab")))

    (t/is (= {:site.fabricate.file/filename "content/test"
              :site.fabricate.file/output-extension "md"
              :site.fabricate.file/template-suffix ".fab"}
             (get-file-metadata "content/test.md.fab")))

    (let [fsm-f (io/file "pages/finite-schema-machines.html.fab")]
      (t/is (= "pages/finite-schema-machines.html.fab"
               (->dir-local-path fsm-f))))))

(t/deftest text-parser
  (t/testing "parsed element schema"
    (t/is
     (m/validate parsed-expr-schema
                 {:expr-src "(+ 3 4)"
                  :expr '(+ 3 4)
                  :error nil
                  :result 7}))
    (t/is
     (m/validate parsed-expr-schema
                 {:expr-src "(+ 3 4)"
                  :exec '(+ 3 4)
                  :error nil
                  :result nil}))
    (t/is
     (m/validate parsed-expr-schema
                 {:expr-src "((+ 3 4)"
                  :expr nil
                  :error {:type clojure.lang.ExceptionInfo
                          :cause "Unexpected EOF while reading item 1 of list."
                          :data {:type :reader-exception :ex-kind :eof}}
                  :result nil}))

    (t/is (m/validate parsed-expr-schema
                      (first (parse "✳+(println \"a form evaluated but displayed without its output\")🔚")))))

  (t/testing "expression parsing"
    (t/is (= ["text " {:expr '(+ 2 3)
                       :expr-src "(+ 2 3)"
                       :display false}]
             (parse "text ✳=(+ 2 3)🔚")))

    (t/is (not (nil? (:error (first (parse "✳((+ 2 3)🔚")))))
          "Expression parsing errors should be surfaced")

    (t/is (= [{:exec '(+ 2 3)
               :expr-src "(+ 2 3)"
               :display false}]
             (parse "✳(+ 2 3)🔚")))

    (t/is (=
           [{:expr-src ":div", :expr :div}
            {:expr-src "{:class \"col\"}", :expr {:class "col"}}
            [:txt "some text"]]
           (extended-form->form
            [:extended-form
             "["
             ":div {:class \"col\"}"
             [:form-contents [:txt "some text"]] "]"])))

    (t/is (m/validate parsed-schema
                      (template "✳//[:div
✳+=(let [s \"output\"]
    [:code (format \"a form evaluated and displayed with its %s\" s)]) 🔚
]//🔚")))

    (t/is (= [{:expr-src ":div", :expr :div} [:txt "\n"] [:expr [:ctrl "+="] "(let [s \"output\"]\n    [:code (format \"a form evaluated and displayed with its %s\" s)]) "] [:txt "\n"]]
             (-> "✳//[:div

✳+=(let [s \"output\"]
    [:code (format \"a form evaluated and displayed with its %s\" s)]) 🔚
]//🔚"
                 template
                 second
                 extended-form->form)))

    (t/is (-> "✳=(+ 2 3)🔚"
              read-template
              second
              parsed-form->expr-map
              meta
              :instaparse.gll/start-line
              some?)
          "Instaparse metadata should be lifted into expr metadata")

    (t/is (some? (meta (first (parse "✳=(+ 2 3)🔚")))))

    (t/is (some? (meta (first (parse "✳//[:div \n more text ]//🔚")))))))

(t/deftest parsed-content-transforms
  (t/testing "namespace retrieval"

    (t/is (= (symbol 'test-ns) (yank-ns (parse "✳(ns test-ns)🔚"))))

    (t/is (nil? (yank-ns (parse "✳=(+ 3 4)🔚")))))

  (t/testing "metadata retrieval"
    (t/is (= '(def metadata {:title "Test" :namespace (ns site.fabricate.demo)})
             (-> "✳(def metadata {:title \"Test\" :namespace (ns site.fabricate.demo)})🔚"
                 parse
                 get-metadata)))

    (t/is (= nil
             (-> "✳(+ 3 4 5)🔚"
                 parse
                 get-metadata)))))

(t/deftest parsed-expression-evaluation
  (t/testing "evaluation of parsed expressions"
    (t/testing ": single exprs"
      (t/is (= 5 (eval-parsed-expr (first (parse "✳=(+ 2 3)🔚")) true)))
      (t/is (= {:expr '(+ 2 3), :expr-src "(+ 2 3)", :error nil, :result 5
                :display false}
               (eval-parsed-expr (first (parse "✳=(+ 2 3)🔚")) false)))
      (t/is (= nil
               (eval-parsed-expr {:exec '(def myvar 3) :expr-src "(def myvar 3)"}
                                 true)))

      (t/is (= {:exec '(def something 23)
                :expr-src "(def something 23)"
                :result [:pre [:code {:class "language-clojure"} "(def something 23)"]]
                :error nil
                :display true}
               (-> "✳+(def something 23)🔚"
                   parse
                   first
                   (eval-parsed-expr false))))

      (t/is
       (= {:expr-src "(+ 4 5)" :display true
           :expr '(+ 4 5) :result 9 :error nil}
          (-> "✳+=(+ 4 5)🔚"
              parse
              first
              (eval-parsed-expr false))))

      (t/is
       (= '([:pre [:code {:class "language-clojure"} "(+ 4 5)\n"]] 9)
          (-> "✳+=(+ 4 5)🔚"
              parse
              first
              (eval-parsed-expr true)))
       "Results of forms should display properly alongside source expressions")

      (t/is (m/validate
             error-form-schema
             [:div
              {:class "fabricate-error"}
              [:h6 "Error"]
              [:dl [:dt "Error type"] [:dd [:code "clojure.lang.ExceptionInfo"]] [:dt "Error message"] [:dd [:code "Unexpected EOF while reading item 1 of list."]]
               [:dt "Error phase"] [:dd [:code ""]]
               [:dt "Location"]
               [:dd
                '("Line " [:strong 1] ", " "Columns " [:strong 1 "-" 12])]]
              [:details [:summary "Source expression"] [:pre [:code "((+ 2 3)"]]]]))

      (t/is
       (=
        [:div {:class "fabricate-error"}
         [:h6 "Error"]
         [:dl
          [:dt "Error type"]
          [:dd
           {:class "fabricate-error-type"}
           [:code "clojure.lang.ExceptionInfo"]]
          [:dt "Error message"]
          [:dd
           {:class "fabricate-error-msg"}
           [:code "Unexpected EOF. [at line 1, column 9]"]]
          [:dt "Error phase"]
          [:dd
           {:class "fabricate-error-phase"}
           [:code ""]]
          [:dt "Location"]
          [:dd
           {:class "fabricate-error-location"}
           '("Line " [:strong 1] ", " "Columns " [:strong 1 "-" 12])]]
         [:details
          [:summary "Source expression"]
          [:pre [:code {:class "language-clojure fabricate-error-src"}
                 "((+ 2 3)"]]]]
        (eval-parsed-expr (first (parse "✳((+ 2 3)🔚")) true))
       "Expression parsing errors should be surfaced in the output")

      (t/is (not (nil? (:error (eval-parsed-expr (first (parse "✳=((+ 2 3)🔚")) false))))))

    (t/testing ": multiple exprs"
      (t/is (= [:foo " bar " :baz]
               (parse-eval "✳=:foo🔚 bar ✳=:baz🔚")))
      (t/is (= ["some text"] (parse-eval "some text"))
            "Plaintext should be passed as-is")

      (t/is (= [[1 2 3]] (parse-eval "✳=[1 2 3]🔚")))
      (t/is (= [["a" "b"]] (parse-eval "✳=[\"a\" \"b\"]🔚"))
            "Escaped quotes in forms should be preserved.")
      (t/is (= [nil " foo " 3]  (eval-all  (parse "✳(def var 3)🔚 foo ✳=var🔚")  'var-test-ns))
            "In-form defs should be evaluated successfully.")

      (t/is (= [[:em 3]]
               (parse-eval "✳=(site.fabricate.prototype.page/em 3)🔚"))
            "Namespace scoping should be preserved")

      (t/is (= [[:em 3]] (parse-eval "✳=(em 3)🔚"))
            "Namespace scoping should be preserved")

      (t/is (= [[:em "text"] ", with a comma following"]
               (parse-eval "✳=[:em \"text\"]🔚, with a comma following")))

      (t/is (= (parse-eval "✳=:foo🔚 bar ✳=:baz🔚")
               [:foo " bar " :baz]))
      (t/is (= (parse-eval "some text") ["some text"])
            "Plaintext should be passed as-is")
      (t/is (= (parse-eval "✳=[1 2 3]🔚") [[1 2 3]]))
      (t/is (= (parse-eval "✳=[\"a\" \"b\"]🔚")  [["a" "b"]])
            "Escaped quotes in forms should be preserved.")
      (t/is (= [nil " baz " nil " foo " 3]
               (let [parsed (parse "✳(ns test-form-ns)🔚 baz ✳(def var 3)🔚 foo ✳=var🔚")]
                 (eval-all parsed)))
            "In-form defs should be evaluated successfully.")

      (t/is (= [[:figure [:img {:src "https://upload.wikimedia.org/wikipedia/commons/9/90/Pterodroma_mollis_light_morph_-_SE_Tasmania_2019.jpg"}]
                 [:figcaption "soft-plumaged petrel"]]]
               (->  "✳=[:figure [:img {:src \"https://upload.wikimedia.org/wikipedia/commons/9/90/Pterodroma_mollis_light_morph_-_SE_Tasmania_2019.jpg\"} ]
                [:figcaption \"soft-plumaged petrel\"]]🔚"
                    parse
                    eval-all))
            "evaluation should not remove content from forms")

      (let [ex-file (-> "README.md.fab"
                        slurp
                        (parse {:filename "README.md.fab"})
                        eval-all)
            ex-meta (-> 'site.fabricate.docs.readme/metadata
                        resolve
                        meta
                        (select-keys [:file :ns :column]))]
        (t/is (=  {:file "README.md.fab" :ns (find-ns 'site.fabricate.docs.readme)
                   :column 1}
                  ex-meta)
              "Vars should preserve information about their source files"))

      (t/is (= (parse-eval "✳=(site.fabricate.prototype.page/em 3)🔚")
               [[:em 3]])
            "Namespace scoping should be preserved")
      (t/is (= (parse-eval "✳=(em 3)🔚")
               [[:em 3]])
            "Namespace scoping should be preserved")

      (t/is (= [[:em "text"] ", with a comma following"]
               (parse-eval "✳=[:em \"text\"]🔚, with a comma following")))

      (t/is (= (hiccup/html
                (apply conj [:div] (parse-eval "✳=[:em \"text\"]🔚, with a comma following")))
               "<div><em>text</em>, with a comma following</div>")))

    (t/testing ": error messages"
      (t/is
       (= [:div 5]
          (eval-all [:div {:expr '(+ 2 3), :expr-src "✳=(+ 2 3)🔚", :error nil, :result nil}]))))))

(t/deftest source-code-transforms
  (t/testing "source printing"
    (t/is (=
           "(def something &quot;abc&quot;)\n"
           (render-src '(do (def something "abc"))
                       true)))

    (t/is (=
           "(def ex-form &quot;a form evaluated but displayed without its output&quot;)\n"
           (render-src '(do (def ex-form "a form evaluated but displayed without its output") nil) true)
           (-> "✳+(def ex-form \"a form evaluated but displayed without its output\")🔚"
               parse
               first
               :exec
               (render-src true))))

    (t/is (=
           [[:pre [:code {:class "language-clojure"} "(println \"a form evaluated but displayed without its output\")"]]]
           (-> "✳+(println \"a form evaluated but displayed without its output\")🔚"
               parse
               eval-all)))))
