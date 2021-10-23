(ns site.fabricate.prototype.html-test.generators
  (:require [site.fabricate.prototype.html :as html]
            [malli.core :as m]
            [malli.util :as mu]
            [malli.error :as me]
            [malli.generator :as mg]
            [clojure.test :as t]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.generators :as gen']))

(def nouns ["steel" "referent" "microscope" "antenna"
            "star" "harp" "electron"])

(def atomic-bounded
  (m/schema
   [:orn
    [:bool :boolean]
    [:decimal
     [:double {:gen/gen
               (gen/double*
                {:infinite? false
                 :NaN? false
                 :min -1000
                 :max 1000})}]]
    [:integer [:int {:gen/gen
                     (gen/large-integer* {:min -1000 :max 1000})}]]
    [:text [:string {:gen/gen gen/string-alphanumeric}]]
    [:nil [:fn {:gen/gen (gen/return nil)} nil?]]]))

(def global-attributes-bounded
  (-> html/global-attributes
      (mu/update :class
                 #(mu/update-properties % assoc :gen/elements nouns))
      (mu/update :id
                 #(mu/update-properties % assoc :gen/gen gen/string-alphanumeric))
      (mu/update :itemid
                 #(mu/update-properties % assoc :gen/gen gen/string-alphanumeric))
      (mu/update :itemprop
                 #(mu/update-properties % assoc :gen/gen gen/string-alphanumeric))
      (mu/update :itemref
                 #(mu/update-properties % assoc :gen/gen gen/string-alphanumeric))
      (mu/update :title
                 #(mu/update-properties % assoc :gen/gen gen/string-alphanumeric))
      (mu/update :part
                 #(mu/update-properties % assoc :gen/gen gen/string-alphanumeric))))


(defn hiccup-base-elems
  "Generates a hiccup element with one of the given tags with no nested sequences."
  ([{:keys [tags contents-gen]
     :or {tags (concat html/flow-tags
                       html/phrasing-tags
                       html/heading-tags)
          ;; an error in the rose tree generator for numeric
          ;; values means we have to use only strings as
          ;; child elements
          contents-gen (gen/vector #_(mg/generator atomic-bounded)
                                   (gen/elements nouns)
                                   0 10)}}]
   (gen/let [elem
             (gen/one-of
              [(gen/tuple (gen/elements tags)
                          (mg/generator global-attributes-bounded))
               (gen/vector (gen/elements html/flow-tags) 1)])
             contents contents-gen]
     (apply conj elem contents)))
  ([] (hiccup-base-elems {})))

(def hiccup-base
  (hiccup-base-elems))

(defn hiccup-recursive-elems
  "Recursive version of hiccup generator using bounded-recursive-gen from test.chuck."
  ([{:keys [outer-tags inner-tags contents-gen
            max-breadth max-height]
     :or {outer-tags (concat html/flow-tags
                             html/phrasing-tags html/heading-tags)
          inner-tags (concat html/flow-tags
                             html/phrasing-tags html/heading-tags)
          contents-gen (gen/vector (gen/elements nouns) 0 10)
          max-breadth 5 max-height 5}}]
   (gen'/bounded-recursive-gen
    (fn [inner]
      (gen/one-of
       [inner
        (gen/let [i inner
                  outer (hiccup-base-elems
                         {:tags outer-tags
                          :contents-gen contents-gen})]
          (conj outer i))]))
    (gen/one-of [(mg/generator atomic-bounded)
                 (hiccup-base-elems {:tags inner-tags
                                     :contents-gen contents-gen})])
    max-breadth
    max-height))
  ([] (hiccup-recursive-elems {})))

(def element
  "Generator elements constrained to conform with the malli schema."
  (gen/such-that
   html/element?
   (hiccup-recursive-elems)))


(comment

  (gen/sample
   element
   20)

  )
