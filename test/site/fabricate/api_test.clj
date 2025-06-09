(ns site.fabricate.api-test
  (:require [clojure.test :as t]
            [site.fabricate.api :as api]
            ;; refer the dev ns to ensure multimethods have impls
            [site.fabricate.dev.build]
            [site.fabricate.prototype.test-utils :refer [with-instrumentation]]
            [site.fabricate.prototype.read]
            [site.fabricate.prototype.html]
            [site.fabricate.prototype.hiccup]
            [site.fabricate.prototype.check]
            [site.fabricate.source :as source]
            [site.fabricate.document :as document]
            [site.fabricate.prototype.schema :as s]
            [malli.core :as m]
            [malli.util :as mu]
            [malli.error :as me]
            [babashka.fs :as fs]
            [clojure.java.io :as io]))

;; ensure registry is available
(doseq [[term schema] api/registry] (s/register! term schema))

(defn doc->page
  [{:keys [site.fabricate.document/title site.fabricate.document/data
           site.fabricate.document/id]
    :as   entry}]
  (assoc entry
         :site.fabricate.page/data  data
         :site.fabricate.page/title title
         :site.fabricate.page/id    id))

(t/use-fixtures :once with-instrumentation)

(defmethod api/collect "deps.edn"
  [src opts]
  (let [loc (fs/file (System/getProperty "user.dir"))]
    [{:site.fabricate.source/location (fs/file loc src)
      :site.fabricate.source/file     (fs/file loc src)
      :site.fabricate.api/source      src
      :site.fabricate.source/format   :clojure/deps
      :site.fabricate.document/format :clojure/edn
      :site.fabricate.page/format     :clojure/edn}]))

(defmethod api/build [:clojure/deps :clojure/edn]
  [{:keys [site.fabricate.source/file] :as entry} _opts]
  (assoc entry
         :site.fabricate.document/data
         (clojure.edn/read-string (slurp file))))



(def valid-entry? (m/validator api/entry-schema))
(def explain-entry (m/explainer api/entry-schema))


(defmethod api/produce! [:clojure/edn :clojure/edn]
  [entry _opts]
  (t/is (valid-entry? entry))
  (assoc entry :site.fabricate.page/output "test"))


(t/deftest operations
  (let [tmp-dir (fs/create-temp-dir {:prefix "fabricate-test-"})
        {:keys [site.fabricate.api/entries] :as plan-data}
        (api/plan!
         [(fn [s] (fs/create-dir (fs/path tmp-dir "css")) s)
          (fn [s] (when-not (fs/exists? "docs") (fs/create-dir "docs")) s)]
         {:site.fabricate.api/options {:site.fabricate.page/publish-dir
                                       tmp-dir}})]
    (t/testing "planning"
      (t/is
       (some #(= "deps.edn" (fs/file-name (:site.fabricate.source/file %)))
             entries)
       "Entries without :site.fabricate/outputs key should be passed through")
      (let [valid-entries? (every? valid-entry? entries)]
        (when-not valid-entries?
          (run! #(println (identity (explain-entry %))) entries))
        (t/is valid-entries? "Every entry collected should be valid.")))
    (t/testing "building"
      (doseq [e entries]
        (let [built (try (api/build e {})
                         (catch Exception exc
                           (do (tap> (dissoc (Throwable->map exc) :trace))
                               (tap> e)
                               nil)))]
          (t/is (some? (:site.fabricate.document/data built))))))
    (t/testing "assembly"
      (let [assembled (api/assemble [#(mapv doc->page
                                            (:site.fabricate.api/entries %))]
                                    {:site.fabricate.api/entries entries})]
        (t/is (contains? (first assembled) :site.fabricate.page/data))))
    (t/testing "production"
      (t/testing "for individual entries"
        (doseq [e entries]
          (let [built (api/build e {})
                _ (do (t/is (some? (#'api/produce-dispatch built {}))))]
            (when-not (= :clojure/deps (:site.fabricate.source/format e))
              (let [{:keys [site.fabricate.page/output] :as produced}
                    (api/produce! built {})]
                (t/is (some? (:site.fabricate.page/title produced))
                      (format "Page produced from %s should have a title"
                              (str (:site.fabricate.source/location built))))
                (t/is (instance? java.io.File output))
                (t/is (fs/exists? output)))))))
      (t/testing "using api/construct!"
        (let [{constructed-entries :site.fabricate.api/entries
               :as constructed-site}
              (->> {:site.fabricate.api/entries entries}
                   (api/assemble [])
                   (api/construct! []))]
          (t/is (map? constructed-site))
          (doseq [{:keys [site.fabricate.page/output] :as e}
                  constructed-entries]
            (t/testing (str "- " (:site.fabricate.source/location e))
              (when-not (or (string? output) (s/file? output)) (println e))
              (t/is
               (or (string? output) (s/file? output))
               "Entries should be updated by api/produce! and api/construct!"))))))))

(defn unmap-multimethods
  "Utility function to ease reloading of redefined multimethods."
  []
  (run! #(ns-unmap (find-ns 'site.fabricate.api) %) '[collect build produce!]))

(comment
  (unmap-multimethods)
  (.getMethodTable api/produce!)
  (api/build! {})
  (fs/create-temp-dir {:prefix "something-"})
  (first built-posts-test))

(def api-contracts
  (let [entry-schema
        [:map [:site.fabricate.api/source :site.fabricate.api/source]
         [:site.fabricate.source/location :site.fabricate.source/location]
         [:site.fabricate.page/output {:optional true}
          :site.fabricate.page/output]
         [:site.fabricate.document/data {:optional true}
          :site.fabricate.document/data]
         [:site.fabricate.document/format {:optional true}
          :site.fabricate.document/format]
         [:site.fabricate.source/format :site.fabricate.source/format]
         [:site.fabricate.page/format {:optional true}
          :site.fabricate.page/format]
         [:site.fabricate.page/uri {:optional true} :site.fabricate.page/uri]
         [:site.fabricate.page/title {:optional true}
          :site.fabricate.page/title]
         [:site.fabricate.page/permalink {:optional true}
          :site.fabricate.page/permalink]
         [:site.fabricate.page/description {:optional true}
          :site.fabricate.page/description]
         [:site.fabricate.page/author {:optional true}
          :site.fabricate.page/author]
         [:site.fabricate.page/language {:optional true}
          :site.fabricate.page/language]
         [:site.fabricate.page/locale {:optional true}
          :site.fabricate.page/locale]
         [:site.fabricate.page/image {:optional true}
          :site.fabricate.page/image]
         [:me.ogp/type {:optional true} :me.ogp/type]
         [:site.fabricate.page/published-time {:optional true}
          :site.fabricate.page/published-time]
         [:site.fabricate.page/publish-dir {:optional true}
          :site.fabricate.page/publish-dir]
         [:site.fabricate.source/created {:optional true}
          :site.fabricate.source/created]
         [:site.fabricate.source/modified {:optional true}
          :site.fabricate.source/modified]
         [:site.fabricate.page/modified-time {:optional true}
          :site.fabricate.page/modified-time]
         [:site.fabricate.page/tags {:optional true} :site.fabricate.page/tags]
         [:site.fabricate.entry/id {:optional true} :site.fabricate.entry/id]
         [:site.fabricate.entry/namespace {:optional true}
          :site.fabricate.entry/namespace]]
        site-fn-schema
        [:schema
         {:registry
          {:site.fabricate.api/task-list
           [:or [:* [:schema [:ref :site.fabricate.api/site-fn]]]
            [:map-of [:schema [:ref :site.fabricate.api/site-fn]]
             [:schema [:ref :site.fabricate.api/site-fn]]]]
           :site.fabricate.api/site-fn
           [:=>
            [:cat [:schema [:ref :site.fabricate.api/task-list]]
             [:schema
              [:map
               [:site.fabricate.api/entries
                [:*
                 [:map [:site.fabricate.api/source :site.fabricate.api/source]
                  [:site.fabricate.source/location
                   :site.fabricate.source/location]
                  [:site.fabricate.page/output {:optional true}
                   :site.fabricate.page/output]
                  [:site.fabricate.document/data {:optional true}
                   :site.fabricate.document/data]
                  [:site.fabricate.document/format {:optional true}
                   :site.fabricate.document/format]
                  [:site.fabricate.source/format :site.fabricate.source/format]
                  [:site.fabricate.page/format {:optional true}
                   :site.fabricate.page/format]
                  [:site.fabricate.page/uri {:optional true}
                   :site.fabricate.page/uri]
                  [:site.fabricate.page/title {:optional true}
                   :site.fabricate.page/title]
                  [:site.fabricate.page/permalink {:optional true}
                   :site.fabricate.page/permalink]
                  [:site.fabricate.page/description {:optional true}
                   :site.fabricate.page/description]
                  [:site.fabricate.page/author {:optional true}
                   :site.fabricate.page/author]
                  [:site.fabricate.page/language {:optional true}
                   :site.fabricate.page/language]
                  [:site.fabricate.page/locale {:optional true}
                   :site.fabricate.page/locale]
                  [:site.fabricate.page/image {:optional true}
                   :site.fabricate.page/image]
                  [:me.ogp/type {:optional true} :me.ogp/type]
                  [:site.fabricate.page/published-time {:optional true}
                   :site.fabricate.page/published-time]
                  [:site.fabricate.page/publish-dir {:optional true}
                   :site.fabricate.page/publish-dir]
                  [:site.fabricate.source/created {:optional true}
                   :site.fabricate.source/created]
                  [:site.fabricate.source/modified {:optional true}
                   :site.fabricate.source/modified]
                  [:site.fabricate.page/modified-time {:optional true}
                   :site.fabricate.page/modified-time]
                  [:site.fabricate.page/tags {:optional true}
                   :site.fabricate.page/tags]
                  [:site.fabricate.entry/id {:optional true}
                   :site.fabricate.entry/id]
                  [:site.fabricate.entry/namespace {:optional true}
                   :site.fabricate.entry/namespace]]]]
               [:site.fabricate.api/options :map]]]]
            [:schema
             [:map
              [:site.fabricate.api/entries
               [:*
                [:map [:site.fabricate.api/source :site.fabricate.api/source]
                 [:site.fabricate.source/location
                  :site.fabricate.source/location]
                 [:site.fabricate.page/output {:optional true}
                  :site.fabricate.page/output]
                 [:site.fabricate.document/data {:optional true}
                  :site.fabricate.document/data]
                 [:site.fabricate.document/format {:optional true}
                  :site.fabricate.document/format]
                 [:site.fabricate.source/format :site.fabricate.source/format]
                 [:site.fabricate.page/format {:optional true}
                  :site.fabricate.page/format]
                 [:site.fabricate.page/uri {:optional true}
                  :site.fabricate.page/uri]
                 [:site.fabricate.page/title {:optional true}
                  :site.fabricate.page/title]
                 [:site.fabricate.page/permalink {:optional true}
                  :site.fabricate.page/permalink]
                 [:site.fabricate.page/description {:optional true}
                  :site.fabricate.page/description]
                 [:site.fabricate.page/author {:optional true}
                  :site.fabricate.page/author]
                 [:site.fabricate.page/language {:optional true}
                  :site.fabricate.page/language]
                 [:site.fabricate.page/locale {:optional true}
                  :site.fabricate.page/locale]
                 [:site.fabricate.page/image {:optional true}
                  :site.fabricate.page/image]
                 [:me.ogp/type {:optional true} :me.ogp/type]
                 [:site.fabricate.page/published-time {:optional true}
                  :site.fabricate.page/published-time]
                 [:site.fabricate.page/publish-dir {:optional true}
                  :site.fabricate.page/publish-dir]
                 [:site.fabricate.source/created {:optional true}
                  :site.fabricate.source/created]
                 [:site.fabricate.source/modified {:optional true}
                  :site.fabricate.source/modified]
                 [:site.fabricate.page/modified-time {:optional true}
                  :site.fabricate.page/modified-time]
                 [:site.fabricate.page/tags {:optional true}
                  :site.fabricate.page/tags]
                 [:site.fabricate.entry/id {:optional true}
                  :site.fabricate.entry/id]
                 [:site.fabricate.entry/namespace {:optional true}
                  :site.fabricate.entry/namespace]]]]
              [:site.fabricate.api/options :map]]]]}}
         :site.fabricate.api/site-fn]]
    {#'api/collect    (m/schema [:=> [:cat :site.fabricate.api/source :map]
                                 :site.fabricate.api/source])
     #'api/plan!      site-fn-schema
     #'api/build      (m/schema [:=> [:cat entry-schema :map]
                                 [:tuple :site.fabricate.source/format
                                  :site.fabricate.document/format]])
     #'api/assemble   site-fn-schema
     #'api/produce!   (m/schema [:=> [:cat entry-schema :map]
                                 [:tuple :site.fabricate.document/format
                                  :site.fabricate.page/format]])
     #'api/construct! site-fn-schema}))


;; making sure the API contract is stable probably means that
;; optional keys should be dropped when making the comparison.

;; the schema for entries should be open for extension, closed for
;; modification.
;; when the API is stable, the schema for required keys should not change,
;; - should the schema for optional keys change either?

;; this gives me an idea: add an explanation of validating entries
;; to the docs

(t/deftest contracts
  (doseq [[v schema] api-contracts]
    (t/testing (str v)
      (let [var-schema (:malli/schema (meta v))]
        (t/is (mu/equals schema var-schema) "API contract must be stable.")))))

(defn test-namespace
  [nmspc]
  (let [ns-str  (str nmspc)
        publics (ns-publics nmspc)]
    (t/testing ns-str
      (let [ns-meta (meta nmspc)]
        (t/is (string? (:doc ns-meta)) "Namespace should be documented")
        (doseq [[sym v] publics]
          (let [v-meta (meta v)]
            (t/is (string? (:doc v-meta)) (str v " should be documented"))))))))

(t/deftest documentation
  (t/testing "documentation across APIs:"
    (->> (all-ns)
         (filter (fn [nmspc]
                   (let [ns-str (str nmspc)]
                     (and (re-find #"^site\.fabricate" ns-str)
                          (not (re-find #"^site\.fabricate\.adorn" ns-str))
                          (not (re-find #"^site\.fabricate\.dev" ns-str))
                          (not (re-find #"^site\.fabricate.*test" ns-str))
                          (not (re-find #"^site\.fabricate.example" ns-str))
                          (not (re-find #"^site\.fabricate.*docs" ns-str))
                          (not (re-find #"^site\.fabricate.*time" ns-str))))))
         (run! test-namespace))))

(comment
  (meta (find-ns 'site.fabricate.api))
  (ns-publics (find-ns 'site.fabricate.prototype.schema)))
