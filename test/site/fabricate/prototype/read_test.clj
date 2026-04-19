(ns site.fabricate.prototype.read-test
  (:require [clojure.test :as t]
            [malli.core :as m]
            [malli.instrument :as mi]
            [malli.util :as mu]
            [malli.error :as me]
            [malli.transform :as mt]
            [hiccup.core :as hiccup]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [site.fabricate.adorn :as adorn]
            [site.fabricate.api :as api]
            [site.fabricate.prototype.test-utils :as tu]
            [matcher-combinators.test]
            [matcher-combinators.matchers :as matchers]
            [site.fabricate.prototype.kindly]
            [site.fabricate.prototype.eval :as prototype.eval]
            [site.fabricate.prototype.read.grammar :refer [template]]
            [site.fabricate.prototype.read :refer :all]))


(defn get-form-vals
  [r]
  (mapv (fn [i]
          (if (#'fabricate-expr? i)
            (cond (:kindly/hide-value i) nil
                  :default (:value i))
            i))
        r))

;; TODO: replace this and refactor tests to avoid this


(defn parse-eval
  [v]
  (->> v
       parse
       eval-all
       get-form-vals))

(comment
  (form? {:code  ":foo"
          :form  :foo
          :value :foo
          :kindly/hide-code true
          :kindly/hide-result false})
  (m/explain site.fabricate.prototype.kindly/Form
             {:code  ":foo"
              :form  :foo
              :value :foo
              :kindly/hide-code true
              :kindly/hide-result false}))

(defn setup [f] (mi/collect!) (mi/instrument!) (f) (mi/unstrument!))

(t/use-fixtures :once #_setup)


(t/deftest file-utils
  (t/testing "Filename utilities"
    (t/is (= {:site.fabricate.file/input-filename   "README"
              :site.fabricate.file/output-extension "md"
              :site.fabricate.file/template-suffix  ".fab"}
             (get-file-metadata "./README.md.fab")))
    (t/is
     (match?
      {:site.fabricate.file/input-filename
       "pages/reference/namespaces/site.fabricate.prototype.write"
       :site.fabricate.file/output-extension "html"
       :site.fabricate.file/template-suffix ".fab"}
      (get-file-metadata
       "./pages/reference/namespaces/site.fabricate.prototype.write.html.fab")))
    (t/is (match? {:site.fabricate.file/input-filename   "some.dir/docs/README"
                   :site.fabricate.file/output-extension "md"
                   :site.fabricate.file/template-suffix  ".fab"}
                  (get-file-metadata "./some.dir/docs/README.md.fab")))
    (t/is (match? {:site.fabricate.file/input-filename   "content/test"
                   :site.fabricate.file/output-extension "md"
                   :site.fabricate.file/template-suffix  ".fab"}
                  (get-file-metadata "./content/test.md.fab")))
    (t/is (match? {:site.fabricate.file/input-filename   "content/test"
                   :site.fabricate.file/output-extension "md"
                   :site.fabricate.file/template-suffix  ".fab"}
                  (get-file-metadata "content/test.md.fab")))))

;; helper function & decoder to make translation easier
(defn old-map->kindly-map
  "Transform from the previous way of representing Clojure forms to the kindly way"
  [{:keys [display error expr expr-src result exec] :as frm}]
  (-> frm
      (clojure.set/rename-keys {:expr     :form
                                :result   :value
                                :expr-src :code
                                :display  :kindly/hide-code
                                :exec     :form})
      (update :kindly/hide-code not)
      (assoc :kindly/hide-value (if exec true false))))

(def form-decoder
  (m/decoder (mu/update-properties site.fabricate.prototype.kindly/Form
                                   assoc
                                   :decode/translate
                                   old-map->kindly-map)
             (mt/transformer {:name :translate})))


(comment
  (form-decoder
   {:display false :error nil :expr '(+ 2 3) :expr-src "(+ 2 3)" :result 5})
  (form-decoder
   {:display true :error nil :exec '(+ 2 3) :expr-src "(+ 2 3)" :result 5})
  (not nil)
  (m/decode
   (mu/update-properties site.fabricate.prototype.kindly/Form
                         assoc
                         :decode/translate
                         old-map->kindly-map)
   {:display false :error nil :expr '(+ 2 3) :expr-src "(+ 2 3)" :result 5}
   (mt/transformer {:name :translate})))

(t/deftest text-parser
  (t/testing "parsed element schema"
    (tu/check-schema
     prototype.eval/Parsed-Form
     (form-decoder {:expr-src "(+ 3 4)" :expr '(+ 3 4) :error nil :result 7}))
    (tu/check-schema
     prototype.eval/Parsed-Form
     (form-decoder {:expr-src "(+ 3 4)" :exec '(+ 3 4) :error nil :result nil}))
    (tu/check-schema prototype.eval/Parsed-Form
                     (form-decoder
                      {:expr-src "((+ 3 4)"
                       :expr     nil
                       :error    {:type clojure.lang.ExceptionInfo
                                  :cause
                                  "Unexpected EOF while reading item 1 of list."
                                  :data {:type :reader-exception :ex-kind :eof}}
                       :result   nil}))
    (tu/check-schema
     prototype.eval/Parsed-Form
     (form-decoder
      (first
       (parse
        "✳+(println \"a form evaluated but displayed without its output\")🔚")))))
  (t/testing "expression parsing"
    (t/is (fabricate-expr? (first (parse "✳=:foo🔚 bar ✳=:baz🔚")))
          "Predicate should match parsed expressions")
    (t/is (match? ["text "
                   (form-decoder
                    {:expr '(+ 2 3) :expr-src "(+ 2 3)" :display false})]
                  (parse "text ✳=(+ 2 3)🔚")))
    (t/is (read-error? (first (parse "✳((+ 2 3)🔚"))))
    (t/is (not (nil? (:error (first (parse "✳((+ 2 3)🔚")))))
          "Expression parsing errors should be surfaced")
    (t/is (match? [(form-decoder
                    {:exec '(+ 2 3) :expr-src "(+ 2 3)" :display false})]
                  (parse "✳(+ 2 3)🔚")))
    (t/is (match?
           [{:code ":div" :form :div}
            {:code "{:class \"col\"}" :form {:class "col"}} [:txt "some text"]]
           (extended-form->form [:extended-form "[" ":div {:class \"col\"}"
                                 [:form-contents [:txt "some text"]] "]"])))
    (tu/check-schema
     Template
     (template
      "✳//[:div
✳+=(let [s \"output\"]
    [:code (format \"a form evaluated and displayed with its %s\" s)]) 🔚
]//🔚"))
    (t/is
     (match?
      [{:code ":div" :form :div} [:txt "\n"]
       [:expr [:ctrl "+="]
        "(let [s \"output\"]\n    [:code (format \"a form evaluated and displayed with its %s\" s)]) "]
       [:txt "\n"]]
      (->
        "✳//[:div

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
    (t/is (some? (meta (first (parse "✳//[:div \n more text ]//🔚")))))
    (t/is (match? "test.clj"
                  (let [parsed (parse "✳=(+ 2 3)🔚" {:filename "test.clj"})]
                    (get-in parsed [0 :file]))))))

(comment
  (-> (m/explain Template (parse "✳(ns test-ns)🔚"))
      (me/humanize)))

(t/deftest parsed-content-transforms
  (t/testing "schema conformance"
    ;(tu/check-schema Template (parse "✳(ns test-ns)🔚"))
    (t/is (every? #(m/validate prototype.eval/Parsed-Form %)
                  (parse "✳(ns test-ns)🔚"))))
  (t/testing "namespace retrieval"
    (t/is (match? (symbol 'test-ns) (yank-ns (parse "✳(ns test-ns)🔚"))))
    (t/is (nil? (yank-ns (parse "✳=(+ 3 4)🔚")))))
  (t/testing "metadata retrieval"
    (t/is
     (match?
      '(def metadata {:namespace (ns site.fabricate.demo) :title "Test"})
      (->
        "✳(def metadata {:title \"Test\" :namespace (ns site.fabricate.demo)})🔚"
        parse
        get-metadata)))
    (t/is (match? nil
                  (-> "✳(+ 3 4 5)🔚"
                      parse
                      get-metadata)))))

(t/deftest parsed-expression-evaluation
  (t/testing "evaluation of parsed expressions"
    #_(t/testing ": single exprs"
        (t/is (= 5 (eval-parsed-expr (first (parse "✳=(+ 2 3)🔚")) true)))
        (t/is (= {:expr     '(+ 2 3)
                  :expr-src "(+ 2 3)"
                  :error    nil
                  :result   5
                  :display  false}
                 (eval-parsed-expr (first (parse "✳=(+ 2 3)🔚")) false)))
        (t/is (= nil
                 (eval-parsed-expr {:exec     '(def myvar 3)
                                    :expr-src "(def myvar 3)"}
                                   true)))
        (t/is (= {:exec     '(def something 23)
                  :expr-src "(def something 23)"
                  :result   [:pre
                             [:code {:class "language-clojure"}
                              "(def something 23)"]]
                  :error    nil
                  :display  true}
                 (-> "✳+(def something 23)🔚"
                     parse
                     first
                     (eval-parsed-expr false))))
        (t/is (= {:expr-src "(+ 4 5)"
                  :display  true
                  :expr     '(+ 4 5)
                  :result   9
                  :error    nil}
                 (-> "✳+=(+ 4 5)🔚"
                     parse
                     first
                     (eval-parsed-expr false))))
        (t/is
         (= (list [:pre
                   [:code {:class "language-clojure"}
                    (adorn/clj->hiccup "(+ 4 5)")]]
                  9)
            (-> "✳+=(+ 4 5)🔚"
                parse
                first
                (eval-parsed-expr true)))
         "Results of forms should display properly alongside source expressions")
        (t/is
         (m/validate
          Error-Hiccup-Form
          [:div {:class "fabricate-error"} [:h6 "Error"]
           [:dl [:dt "Error type"] [:dd [:code "clojure.lang.ExceptionInfo"]]
            [:dt "Error message"]
            [:dd [:code "Unexpected EOF while reading item 1 of list."]]
            [:dt "Error phase"] [:dd [:code ""]] [:dt "Location"]
            [:dd '("Line " [:strong 1] ", " "Columns " [:strong 1 "-" 12])]]
           [:details [:summary "Source expression"]
            [:pre [:code "((+ 2 3)"]]]]))
        (t/is (= [:div {:class "fabricate-error"} [:h6 "Error"]
                  [:dl {:class "fabricate-error-info"} [:dt "Error type"]
                   [:dd {:class "fabricate-error-type"}
                    [:code "clojure.lang.ExceptionInfo"]] [:dt "Error message"]
                   [:dd {:class "fabricate-error-msg"}
                    [:code "Unexpected EOF. [at line 1, column 9]"]]
                   [:dt "Error phase"]
                   [:dd {:class "fabricate-error-phase"} [:code ""]]
                   [:dt "Location"]
                   [:dd {:class "fabricate-error-location"}
                    '("Line " [:strong 1] ", " "Columns " [:strong 1 "-" 12])]]
                  [:details [:summary "Source expression"]
                   [:pre {:class "fabricate-error-src"}
                    [:code {:class "language-clojure"} "((+ 2 3)"]]]]
                 (eval-parsed-expr (first (parse "✳((+ 2 3)🔚")) true))
              "Expression parsing errors should be surfaced in the output")
        (t/is (not (nil? (:error (eval-parsed-expr (first (parse
                                                           "✳=((+ 2 3)🔚"))
                                                   false))))))
    (t/testing ": multiple exprs"
      (t/is (match? [:foo " bar " :baz] (parse-eval "✳=:foo🔚 bar ✳=:baz🔚")))
      (t/is (match? ["some text"] (parse-eval "some text"))
            "Plaintext should be passed as-is")
      (t/is (match? [[1 2 3]] (parse-eval "✳=[1 2 3]🔚")))
      (t/is (match? [["a" "b"]] (parse-eval "✳=[\"a\" \"b\"]🔚"))
            "Escaped quotes in forms should be preserved.")
      (t/is (match? [nil " foo " 3]
                    (-> "✳(def var 3)🔚 foo ✳=var🔚"
                        (parse)
                        (eval-all true 'var-test-ns)
                        get-form-vals))
            "In-form defs should be evaluated successfully.")
      (let [evaluated (eval-all (parse
                                 "✳(def metadata {:a 3})🔚 foo ✳=metadata🔚")
                                true
                                'var-test-ns)
            form-meta (meta evaluated)]
        (tap> form-meta)
        (tu/check-schema prototype.eval/Evaluated-Form
                         (evaluated 0)
                         "Evaluation should produce valid Kindly forms")
        (t/is (string? (evaluated 1)))
        (tu/check-schema prototype.eval/Evaluated-Form
                         (evaluated 2)
                         "Evaluation should produce valid Kindly forms")
        (t/is (some? (:namespace form-meta))
              "Namespace information should be attached to evaluated form")
        (t/is (match? {:a 3} (:metadata form-meta))
              "Metadata should be attached to evaluated form"))
      (t/is (match? [[:em "text"] ", with a comma following"]
                    (parse-eval "✳=[:em \"text\"]🔚, with a comma following")))
      (t/is (match? (parse-eval "✳=:foo🔚 bar ✳=:baz🔚") [:foo " bar " :baz]))
      (t/is (match? (parse-eval "some text") ["some text"])
            "Plaintext should be passed as-is")
      (t/is (match? (parse-eval "✳=[1 2 3]🔚") [[1 2 3]]))
      (t/is (match? (parse-eval "✳=[\"a\" \"b\"]🔚") [["a" "b"]])
            "Escaped quotes in forms should be preserved.")
      (t/is (match? [nil " baz " nil " foo " 3]
                    (let
                      [parsed
                       (parse
                        "✳(ns test-form-ns)🔚 baz ✳(def var 3)🔚 foo ✳=var🔚")]
                      (get-form-vals (eval-all parsed))))
            "In-form defs should be evaluated successfully.")
      (t/is
       (match?
        [[:figure
          [:img
           {:src
            "https://upload.wikimedia.org/wikipedia/commons/9/90/Pterodroma_mollis_light_morph_-_SE_Tasmania_2019.jpg"}]
          [:figcaption "soft-plumaged petrel"]]]
        (->
          "✳=[:figure [:img {:src \"https://upload.wikimedia.org/wikipedia/commons/9/90/Pterodroma_mollis_light_morph_-_SE_Tasmania_2019.jpg\"} ]
                [:figcaption \"soft-plumaged petrel\"]]🔚"
          parse
          eval-all
          get-form-vals))
       "evaluation should not remove content from forms")
      (let [ex-file (-> "README.md.fab"
                        slurp
                        (parse {:filename "README.md.fab"})
                        eval-all)
            ex-meta (-> 'site.fabricate.docs.readme/metadata
                        resolve
                        meta
                        (select-keys [:file :ns :column]))]
        (t/is (match? {:file   "README.md.fab"
                       :ns     (find-ns 'site.fabricate.docs.readme)
                       :column 1}
                      ex-meta)
              "Vars should preserve information about their source files"))
      #_(t/is (match? (parse-eval "✳=(em 3)🔚") [[:em 3]])
              "Namespace scoping should be preserved")
      (t/is (match? [[:em "text"] ", with a comma following"]
                    (parse-eval "✳=[:em \"text\"]🔚, with a comma following")))
      (t/is (match? (hiccup/html
                     (apply conj
                            [:div]
                            (parse-eval
                             "✳=[:em \"text\"]🔚, with a comma following")))
                    "<div><em>text</em>, with a comma following</div>")))
    (t/testing ": error messages"
      (t/is (match? {:error {:type 'clojure.lang.ExceptionInfo
                             :data {:msg "Unexpected EOF."}}}
                    (first (parse "✳(+ 2 3🔚")))
            "parsing errors should be surfaced by default"))))

(t/deftest source-code-transforms)

(comment
  (parse "✳(+ 2 3🔚")
  (meta #'api/plan!)
  (-> 'site.fabricate.docs.readme/metadata
      resolve
      meta)
  (require '[clojure.pprint])
  (add-tap (bound-fn* clojure.pprint/pprint)))
