(ns site.fabricate.api-test
  (:require [clojure.test :as t]
            [site.fabricate.api :as api]
            [site.fabricate.dev.build] ; refer the dev ns to ensure
                                        ; multimethods have impls
            [site.fabricate.prototype.test-utils :refer [with-instrumentation]]
            [malli.core :as m]
            [malli.util :as mu]
            [babashka.fs :as fs]))

(defn doc->page
  [{:keys [site.fabricate.document/title site.fabricate.document/data
           site.fabricate.document/id],
    :as entry}]
  (assoc entry
    :site.fabricate.page/data data
    :site.fabricate.page/title title
    :site.fabricate.page/id id))

(t/use-fixtures :once with-instrumentation)


(t/deftest api-ops
  (let [tmp-dir (fs/create-temp-dir {:prefix "fabricate-test-"})
        {:keys [site.fabricate.api/entries], :as plan-data}
        (api/plan! [(fn [s] (fs/create-dir (fs/path tmp-dir "css")) s)]
                   {:site.fabricate.api/options
                    {:site.fabricate.page/publish-dir tmp-dir}})]
    (t/testing "planning"
      (t/is (every? (m/validator api/entry-schema) entries)))
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
      (doseq [e entries]
        (let [built (api/build e {})
              _ (do (t/is (some? (api/produce-dispatch built {}))))
              {:keys [site.fabricate.page/output], :as produced}
              (api/produce! built {})]
          (t/is (some? (:site.fabricate.page/title produced))
                (format "Page produced from %s should have a title"
                        (str (:site.fabricate.source/location built))))
          (t/is (instance? java.io.File output))
          (t/is (fs/exists? output)))))))

(defn unmap-multimethods
  "Utility function to ease reloading of redefined multimethods."
  []
  (run! #(ns-unmap (find-ns 'site.fabricate.api) %)
        '[collect build produce!]))

(comment
  (unmap-multimethods)
  (.getMethodTable api/produce!)
  (api/build! {})
  (fs/create-temp-dir {:prefix "something-"})
  (first built-posts-test))

(def api-contracts
  (let
      [entry-schema
       [:map [:site.fabricate.entry/source :site.fabricate.entry/source]
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
        [:site.fabricate.page/title {:optional true} :site.fabricate.page/title]
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
        [:site.fabricate.page/image {:optional true} :site.fabricate.page/image]
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
         #:site.fabricate.api{:task-list
                              [:or
                               [:*
                                [:schema
                                 [:ref
                                  :site.fabricate.api/site-fn]]]
                               [:map-of
                                [:schema
                                 [:ref
                                  :site.fabricate.api/site-fn]]
                                [:schema
                                 [:ref
                                  :site.fabricate.api/site-fn]]]],
                              :site-fn
                              [:=>
                               [:cat
                                [:schema
                                 [:ref
                                  :site.fabricate.api/task-list]]
                                [:schema
                                 [:map
                                  [:site.fabricate.api/entries
                                   [:*
                                    [:map
                                     [:site.fabricate.entry/source
                                      :site.fabricate.entry/source]
                                     [:site.fabricate.source/location
                                      :site.fabricate.source/location]
                                     [:site.fabricate.page/output
                                      {:optional true}
                                      :site.fabricate.page/output]
                                     [:site.fabricate.document/data
                                      {:optional true}
                                      :site.fabricate.document/data]
                                     [:site.fabricate.document/format
                                      {:optional true}
                                      :site.fabricate.document/format]
                                     [:site.fabricate.source/format
                                      :site.fabricate.source/format]
                                     [:site.fabricate.page/format
                                      {:optional true}
                                      :site.fabricate.page/format]
                                     [:site.fabricate.page/uri
                                      {:optional true}
                                      :site.fabricate.page/uri]
                                     [:site.fabricate.page/title
                                      {:optional true}
                                      :site.fabricate.page/title]
                                     [:site.fabricate.page/permalink
                                      {:optional true}
                                      :site.fabricate.page/permalink]
                                     [:site.fabricate.page/description
                                      {:optional true}
                                      :site.fabricate.page/description]
                                     [:site.fabricate.page/author
                                      {:optional true}
                                      :site.fabricate.page/author]
                                     [:site.fabricate.page/language
                                      {:optional true}
                                      :site.fabricate.page/language]
                                     [:site.fabricate.page/locale
                                      {:optional true}
                                      :site.fabricate.page/locale]
                                     [:site.fabricate.page/image
                                      {:optional true}
                                      :site.fabricate.page/image]
                                     [:me.ogp/type
                                      {:optional true}
                                      :me.ogp/type]
                                     [:site.fabricate.page/published-time
                                      {:optional true}
                                      :site.fabricate.page/published-time]
                                     [:site.fabricate.page/publish-dir
                                      {:optional true}
                                      :site.fabricate.page/publish-dir]
                                     [:site.fabricate.source/created
                                      {:optional true}
                                      :site.fabricate.source/created]
                                     [:site.fabricate.source/modified
                                      {:optional true}
                                      :site.fabricate.source/modified]
                                     [:site.fabricate.page/modified-time
                                      {:optional true}
                                      :site.fabricate.page/modified-time]
                                     [:site.fabricate.page/tags
                                      {:optional true}
                                      :site.fabricate.page/tags]
                                     [:site.fabricate.entry/id
                                      {:optional true}
                                      :site.fabricate.entry/id]
                                     [:site.fabricate.entry/namespace
                                      {:optional true}
                                      :site.fabricate.entry/namespace]]]]
                                  [:site.fabricate.api/options
                                   :map]]]]
                               [:schema
                                [:map
                                 [:site.fabricate.api/entries
                                  [:*
                                   [:map
                                    [:site.fabricate.entry/source
                                     :site.fabricate.entry/source]
                                    [:site.fabricate.source/location
                                     :site.fabricate.source/location]
                                    [:site.fabricate.page/output
                                     {:optional true}
                                     :site.fabricate.page/output]
                                    [:site.fabricate.document/data
                                     {:optional true}
                                     :site.fabricate.document/data]
                                    [:site.fabricate.document/format
                                     {:optional true}
                                     :site.fabricate.document/format]
                                    [:site.fabricate.source/format
                                     :site.fabricate.source/format]
                                    [:site.fabricate.page/format
                                     {:optional true}
                                     :site.fabricate.page/format]
                                    [:site.fabricate.page/uri
                                     {:optional true}
                                     :site.fabricate.page/uri]
                                    [:site.fabricate.page/title
                                     {:optional true}
                                     :site.fabricate.page/title]
                                    [:site.fabricate.page/permalink
                                     {:optional true}
                                     :site.fabricate.page/permalink]
                                    [:site.fabricate.page/description
                                     {:optional true}
                                     :site.fabricate.page/description]
                                    [:site.fabricate.page/author
                                     {:optional true}
                                     :site.fabricate.page/author]
                                    [:site.fabricate.page/language
                                     {:optional true}
                                     :site.fabricate.page/language]
                                    [:site.fabricate.page/locale
                                     {:optional true}
                                     :site.fabricate.page/locale]
                                    [:site.fabricate.page/image
                                     {:optional true}
                                     :site.fabricate.page/image]
                                    [:me.ogp/type {:optional true}
                                     :me.ogp/type]
                                    [:site.fabricate.page/published-time
                                     {:optional true}
                                     :site.fabricate.page/published-time]
                                    [:site.fabricate.page/publish-dir
                                     {:optional true}
                                     :site.fabricate.page/publish-dir]
                                    [:site.fabricate.source/created
                                     {:optional true}
                                     :site.fabricate.source/created]
                                    [:site.fabricate.source/modified
                                     {:optional true}
                                     :site.fabricate.source/modified]
                                    [:site.fabricate.page/modified-time
                                     {:optional true}
                                     :site.fabricate.page/modified-time]
                                    [:site.fabricate.page/tags
                                     {:optional true}
                                     :site.fabricate.page/tags]
                                    [:site.fabricate.entry/id
                                     {:optional true}
                                     :site.fabricate.entry/id]
                                    [:site.fabricate.entry/namespace
                                     {:optional true}
                                     :site.fabricate.entry/namespace]]]]
                                 [:site.fabricate.api/options
                                  :map]]]]}}
        :site.fabricate.api/site-fn]]
    {#'api/collect (m/schema [:=> [:cat :site.fabricate.entry/source :map]
                              :site.fabricate.entry/source]),
     #'api/plan! site-fn-schema,
     #'api/build (m/schema [:=> [:cat entry-schema :map]
                            [:tuple :site.fabricate.source/format
                             :site.fabricate.document/format]]),
     #'api/assemble site-fn-schema,
     #'api/produce! (m/schema [:=> [:cat entry-schema :map]
                               [:tuple :site.fabricate.document/format
                                :site.fabricate.page/format]]),
     #'api/construct! site-fn-schema}))


(t/deftest contracts
  (doseq [[v schema] api-contracts]
    (t/testing (str v)
      (let [var-schema (:malli/schema (meta v))]
        (t/is (mu/equals schema var-schema) "API contract must be stable.")))))
