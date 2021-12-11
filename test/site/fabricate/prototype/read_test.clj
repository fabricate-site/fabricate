(ns site.fabricate.prototype.read-test
  (:require  [clojure.test :as t]
             [site.fabricate.prototype.page :refer [em link code blockquote]]
             [malli.core :as m]
             [malli.instrument :as mi]
             [hiccup.core :as hiccup]
             [clojure.java.io :as io]
             [site.fabricate.prototype.read :refer :all]))

(defn setup [f]
  (def parse-eval (comp eval-all parse))
  (require '[site.fabricate.prototype.page :refer [em link]])
  (mi/collect!)
  (mi/instrument!)
  (f)
  (mi/unstrument!))

(t/use-fixtures :once setup)

(t/deftest parser

  (t/testing "parsed element model"
    (t/is
     (m/validate parsed-expr-schema
                 {:src "(+ 3 4)"
                  :expr '(+ 3 4)
                  :err nil
                  :result 7}))
    (t/is
     (m/validate parsed-expr-schema
                 {:src "(+ 3 4)"
                  :exec '(+ 3 4)
                  :err nil
                  :result nil}))
    (t/is
     (m/validate parsed-expr-schema
                 {:src "((+ 3 4)"
                  :expr nil
                  :err {:type clojure.lang.ExceptionInfo
                        :cause "Unexpected EOF while reading item 1 of list."
                        :data {:type :reader-exception :ex-kind :eof}}
                  :result nil}))

    (t/is (m/validate parsed-expr-schema
                      (first (parse "✳+(println \"a form evaluated but displayed without its output\")🔚")))))

  (t/testing "expression parsing"

    (t/is (= ["text " {:expr '(+ 2 3)
                       :src "(+ 2 3)"
                       :display false}]
             (parse "text ✳=(+ 2 3)🔚")))

    (t/is (not (nil? (:err (first (parse "✳((+ 2 3)🔚")))))
          "Expression parsing errors should be surfaced")

    (t/is (= [{:exec '(+ 2 3)
               :src "(+ 2 3)"
               :display false}]
             (parse "✳(+ 2 3)🔚"))))

  (t/testing "evaluation of parsed expressions"
    (t/is (= 5 (eval-parsed-expr (first (parse "✳=(+ 2 3)🔚")) true)))
    (t/is (= {:expr '(+ 2 3), :src "(+ 2 3)", :err nil, :result 5
              :display false}
             (eval-parsed-expr (first (parse "✳=(+ 2 3)🔚")) false)))
    (t/is (= nil
             (eval-parsed-expr {:exec '(def myvar 3) :src "(def myvar 3)"}
                               true)))

    (t/is (= {:exec '(def something 23)
              :src "(def something 23)"
              :result [:pre [:code {:class "language-clojure"} "(def something 23)"]]
              :err nil
              :display true}
             (-> "✳+(def something 23)🔚"
                 parse
                 first
                 (eval-parsed-expr false))))

    (t/is (=
           [{:src ":div", :expr :div}
            {:src "{:class \"col\"}", :expr {:class "col"}}
            [:txt "some text"]]
           (extended-form->form
            [:extended-form
             "["
             ":div {:class \"col\"}"
             [:txt "some text"] "]"])))

    (t/is (and
           (not (nil? (:err (eval-parsed-expr (first (parse "✳=((+ 2 3)🔚")) false))))
           (not (nil? (:err (eval-parsed-expr (first (parse "✳=((+ 2 3)🔚")) true)))))))

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
               (let [parsed (parse "✳(ns test-form-ns)🔚 baz ✳(def var 3)🔚 foo ✳=var🔚")]
                 (eval-all parsed)))
            "In-form defs should be evaluated successfully.")

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
        (eval-all [:div {:expr '(+ 2 3), :src "✳=(+ 2 3)🔚", :err nil, :result nil}]))))

  (t/testing "string parse+eval"

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
             (parse-eval "✳=[:em \"text\"]🔚, with a comma following"))))

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

(comment
  (eval-parsed-expr
   (first
    (parse "✳+(println \"a form evaluated but displayed without its output\")🔚"))))

(t/deftest file-utils
  (t/testing "Filename utilities"
    (t/is (= {:filename "README"
              :file-extension "md"
              :fabricate/suffix ".fab"}
             (get-file-metadata "./README.md.fab")))

    (t/is (= {:filename "some.dir/docs/README"
              :file-extension "md"
              :fabricate/suffix ".fab"}
             (get-file-metadata "./some.dir/docs/README.md.fab")))

    (t/is (= {:filename "content/test"
              :file-extension "md"
              :fabricate/suffix ".fab"}
             (get-file-metadata "./content/test.md.fab")))

    (t/is (= {:filename "content/test"
              :file-extension "md"
              :fabricate/suffix ".fab"}
             (get-file-metadata "content/test.md.fab")))

    (let [fsm-f (io/file "pages/finite-schema-machines.html.fab")]
      (t/is (= "pages/finite-schema-machines.html.fab"
               (->dir-local-path fsm-f))))))
