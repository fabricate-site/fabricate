(ns site.fabricate.dev.code-test
  (:require [site.fabricate.dev.code :as code]
            [clojure.test :as t]))


(t/deftest escape-fns
  (t/testing "CSS escaping"
    (t/is (string? (re-matches code/css-ident-pattern
                               "abc-def_ghi-123\\.λΩ𝛌🔥"))
          "CSS Ident pattern should match specification")
    (t/is (= "\\#\\'abc\\/def" (code/css-escape "#'abc/def")))))
