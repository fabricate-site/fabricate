(ns site.fabricate.prototype.properties
  "Namespace defining checks and properties that should hold of each entry."
  (:require [clojure.test :as t]
            [babashka.fs :as fs]
            [malli.core :as m]
            [malli.util :as mu]
            [malli.error :as me]
            [malli.registry :as mr]
            [site.fabricate.prototype.eval :as eval]
            [matcher-combinators.matchers :as match]
            [matcher-combinators.test]))

;; (defn dir-exists? [_] false)
;; (defn file-exists? [_] false)
;; (defn absolute-path? [_] false)
;; (defn valid-config? [_] false)
;; (defn source-parses? [_] false)
;; (defn unexpected-errors? [_] false)
;; (defn valid-kindly-hiccup? [_] false)
;; (defn no-kindly-maps? [_] false)
;; (defn all-links-resolve? [_] false)
;; (defn has-title? [_] false)

(def FileExists
  "schema for a file that exists"
  (m/schema [:fn {:error/fn (fn [v _] (str (:value v) " should exist"))}
             fs/exists?]))
(def Directory
  "schema for a directory"
  (m/schema [:fn
             {:error/message "should be directory"
              :error/fn      (fn [{:keys [value]} _]
                               (str value " should be directory"))}
             fs/directory?]))
(def AbsolutePath
  "schema for absolute path"
  (m/schema [:fn
             {:error/message "should be absolute"
              :error/fn      (fn [v _] (str (:value v) " should be absolute"))}
             fs/absolute?]))
(def AbsoluteFile
  "schema for absolute file path with existing file"
  (m/schema [:and
             {:description "existing file found at absolute path"
              :error/fn    (fn [v _]
                             (str (:value v)
                                  " should exist at an absolute path"))}
             [:ref #'FileExists] [:ref #'AbsolutePath]]))
(def AbsoluteDirectory
  "schema for absolute directory that exists"
  (m/schema [:and
             {:description "absolute directory that exists"
              :error/fn    (fn [v _]
                             (str (:value _)
                                  " should exist at an absolute path"))}
             #'Directory #'AbsolutePath]))

(def CollectedEntry
  "schema for Fabricate entry returned from site.fabricate.api/collect"
  (m/schema [:map
             {:title "Collected entry"
              :description
              "Fabricate entry returned from site.fabricate.api/collect"}
             [:site.fabricate.source/location #'AbsoluteFile]
             [:site.fabricate.source/directory #'AbsoluteDirectory]]))

(def BuiltEntry
  "schema for Fabricate entry returned from site.fabricate.api/build"
  (mu/merge CollectedEntry
            [:map
             {:title "Built entry"
              :description
              "Fabricate entry returned from site.fabricate.api/build"}
             [:site.fabricate.document/title :string]
             [:site.fabricate.document/data :any]]))

(def ProducedEntry
  "schema for Fabricate entry returned from site.fabricate.api/produce!"
  nil)

(def evaluated-kindly-map?
  "returns true if the value is an evaluated Kindly map."
  (m/validator eval/Evaluated-Form))

(defn classify-render-output
  "Determine whether the render output has the expected components based on Kindly metadata"
  [{:keys [kind value kindly/hide-value kindly/hide-code] :as form} output]
  (cond (and hide-code hide-value (some? output)) :unexpected-output
        (and (nil? output) (some? value) (not hide-value))
        :unexpected-nil-output
        (evaluated-kindly-map? output) :kindly-form
        :else :ok))

(def ^:private render-error-messages
  {:unexpected-output
   "render returns output when hide-code and hide-value are both true"
   :unexpected-nil-output
   "render returns nil when value exists and is not hidden"
   :kindly-form "render returns kindly form instead of rendered value"
   :ok "render returns rendered value"})

(defn- valid-render-output?
  [[[form] output]]
  (= :ok (classify-render-output form output)))

(defn- render-output-error-fn
  [[[form] output]]
  (get render-error-messages (classify-render-output form output)))

(def =>RenderForm
  "function schema for site.fabricate.api/render-form output"
  (m/schema [:=> [:cat eval/Evaluated-Form :map] :any
             [:fn {:error/fn render-output-error-fn} valid-render-output?]]))

(comment
  (me/humanize (m/explain AbsoluteDirectory
                          (fs/file "/path/to/non-existent/dir")))
  (fs/directory? (fs/file "./src"))
  (me/humanize (m/explain [:fn
                           {:title         "File exists"
                            :error/message "file should exist"
                            :error/fn      (fn [{:keys [value]} _]
                                             (str value " should exist"))}
                           fs/exists?]
                          "./path/to-nonexistent-file.txt")))
