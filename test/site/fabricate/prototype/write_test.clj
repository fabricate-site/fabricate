(ns site.fabricate.prototype.write-test
  (:require [site.fabricate.prototype.write :refer :all]
            [malli.core :as m]
            [clojure.test :as t]))

(t/deftest page-fsm
  (t/testing "page metadata"
    (t/is (= {:namespace (symbol 'test-ns)
              :page-content [{:expr '(do (ns test-ns) nil)
                              :src "✳(ns test-ns)🔚"
                              :err nil
                              :result nil}]
              :output-file "./pages/test-file"
              :input-file "./content/test-file"}
             (populate-page-meta
              {:page-content [{:expr '(do (ns test-ns) nil)
                               :src "✳(ns test-ns)🔚"
                               :err nil
                               :result nil}]
               :input-file "./content/test-file"}
              {:output-dir "./pages"})))

    )

  (t/is (m/validate (-> populate-page-meta
                        var
                        meta
                        :malli/schema)
                    populate-page-meta))

  )
