(ns site.fabricate.dev.references-test
  (:require [site.fabricate.dev.references :as refs]
            [clojure.test :as t]))

(t/deftest specification
  (t/testing "Reference specification"
    (doseq [term [#'site.fabricate.api/plan! 'site.fabricate.api/construct!
                  :basic-kw :site.fabricate.source/location]]
      (t/testing (str term)
        (t/is (map? (refs/specify term))
              "Basic term types should be specifiable")))))
