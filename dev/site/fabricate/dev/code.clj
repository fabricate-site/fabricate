(ns site.fabricate.dev.code
  "Tools for working with Clojure source code and metadata"
  (:require [site.fabricate.adorn :as adorn :refer [clj->hiccup]]
            [site.fabricate.prototype.schema :as schema]
            [rewrite-clj.zip :as z]
            [rewrite-clj.parser :as p]
            [rewrite-clj.node :as node]
            [malli.core :as m]
            [malli.util :as mu]
            [clojure.string :as str]))

(defn namespace? [val] (= clojure.lang.Namespace (class val)))

(def var-metadata-schema
  "Malli schema for var metadata"
  (m/schema
   [:map {:doc "Schema for var metadata"}
    [:doc {:optional? true :description "Docstring for the var"} :string]
    [:line
     {:description
      "The starting line of the definition of the var in its source file"} :int]
    [:column
     {:description
      "The starting column of the definition of the var in its source file"}
     :int] [:file {:description "The source file defining the var"} :string]
    [:name {:description "The name of the var, without its namespace"} :symbol]
    [:ns {:description "The namespace that contains the var"} [:fn namespace?]]
    [:malli/schema
     {:optional? true :description "The Malli schema defining for the var"}
     [:fn schema/malli?]]]))

(defn- zipper? [val] (some? (:rewrite-clj.zip/opts val)))

(def src-schema
  (m/schema [:orn [:rewrite-clj/node [:fn node/node?]]
             [:rewrite-clj/zipper [:fn zipper?]] [:src-string :string]
             [:expr :any]]))

;; Note on CSS escaping: an ID can contain characters like dots and slashes and
;; still be valid. It doesn't need to be escaped to be used as a HTML id, it
;; just needs to be escaped to be selected properly from CSS.
;; The need for escaping can potentially be sidestepped with an
;; attribute selector on the ID value, but that may not work consistently
;; across browsers.

(def css-ident-pattern
  #"(?:[A-Za-z0-9\-_]|(?:\\[^\p{XDigit}])|[\x{00A0}-\x{10FFFF}])+")

;; strategy: do some explicit replacements but then fall back
;; to adding a backslash for unknown characters
(defn css-escape
  "Escape the given string so it can be used as a CSS <ident> selector"
  [string]
  (-> string
      #_(str/replace #"/" "_")
      (str/replace #"([\p{ASCII}&&[^A-Za-z0-9\-_]])" "\\\\$1")))

(def enriched-var-metadata-schema
  "Malli schema for var metadata after enrichment"
  (mu/merge var-metadata-schema
            (m/schema [:map [:clojure/source src-schema] [:var [:fn var?]]
                       [:escaped-fqn :string]])))


(defn include-form
  "Return the form in the file matching the predicate as a rewrite-clj node."
  {:malli/schema [:=> [:cat :string [:fn fn?]] :any]}
  [src-file pred]
  (let [fzip (z/of-file src-file)]
    (clj->hiccup (z/node (z/find-next-depth-first fzip #(pred (z/sexpr %)))))))



(comment
  (class (find-ns 'site.fabricate.api))
  (meta #'site.fabricate.api/collect))
