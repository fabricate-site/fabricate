(ns site.fabricate.prototype.page-test
  (:require [site.fabricate.prototype.page :as page]
            [site.fabricate.prototype.html :as html]
            [site.fabricate.api :as api]
            [site.fabricate.source-test]
            [site.fabricate.document-test]
            [malli.core :as m]
            [malli.error :as me]
            [babashka.fs :as fs]
            [clojure.test :as t]))

(def valid-block? (:site.fabricate.prototype.html/pre html/element-validators))
(def valid-element? (m/validator html/element))

(t/use-fixtures
 :once
 (fn [f] (require '[site.fabricate.prototype.html :as html] :reload) (f)))

(t/deftest kinds
  (doseq [value [:a '(+ 1 2 3) 45 {:b 3}]]
    (t/testing (str "value: " value)
      (t/testing "code"
        (t/is (valid-block? (api/display-form {:kind  :code
                                               :value value
                                               :site.fabricate.page/format
                                               :hiccup/html}))))
      (t/testing "edn"
        (t/is (valid-block? (api/display-form {:kind  :edn
                                               :value value
                                               :site.fabricate.page/format
                                               :hiccup/html}))))))
  (t/testing "hiccup"
    (doseq [hiccup [[:span "text"] [:code "(map dec (range 29 58 2))"]]]
      (t/is (valid-element? (api/display-form {:kind  :hiccup
                                               :value hiccup}))))))

(t/deftest post-processing
  (t/testing "Rendering kindly forms"
    (t/testing "Clojure sources"
      (doseq [{src-loc :site.fabricate.source/location :as e}
              (api/collect "**/*.clj"
                           {:site.fabricate.source/location (fs/file ".")})]
        (t/testing (str "entry " src-loc)
          (let [built-entry (api/build e {})]
            (t/is (some? (page/process-kinds (:site.fabricate.document/data
                                              built-entry)
                                             :hiccup/html)))
            (t/is (map? (page/render-hiccup-article built-entry {})))))))
    #_(t/testing "Fabricate templates"
        (doseq [e (api/collect "**/*.fab"
                               {:site.fabricate.source/location (fs/file ".")})]
          (t/testing (str "entry " (:site.fabricate.source/location e))
            (let [built-entry (api/build e {})]
              (t/is (some? (page/process-kinds (:site.fabricate.document/data
                                                built-entry)
                                               :hiccup/html)))
              (t/is (map? (page/render-hiccup-article built-entry)))))))))


(comment
  (require '[clojure.tools.reader])
  (read-string {} "::build/test-kw"))
