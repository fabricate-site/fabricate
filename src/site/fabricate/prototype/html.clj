(ns site.fabricate.prototype.html
  "Namespace for creating HTML forms using Hiccup data structures and
   for verifying their structural correctness using malli schemas."
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.zip :as zip]
   [clojure.data.finger-tree :as ftree :refer
    [counted-double-list ft-split-at ft-concat]]
   [clojure.test.check.generators :as gen]
   [com.gfredericks.test.chuck.generators :as gen']
   [malli.core :as m]
   [malli.generator :as mg]
   [malli.registry :as mr]
   [malli.dot :as md]
   [malli.error :as me]
   [malli.util :as mu]
   [site.fabricate.prototype.schema :as schema]))

(def block-level-tags
  "MDN list of block-level HTML element tags"
  #{:address :article :aside :blockquote
    :details :dialog :dd :div :dl :dt
    :fieldset :figcaption :figure :footer
    :form :h1 :h2 :h3 :h4 :h5 :h6 :header
    :hr :li :main :nav :ol :p :pre :section
    :table :ul})

(def inline-tags
  "MDN list of inline HTML element tags"
  #{:a :abbr :b :bdi :bdo :br #_:button
    :canvas :cite :code :data #_:datalist
    :del :dfn :em #_:embed :i #_:iframe :img
    #_:input :ins :kbd #_:label :mark #_:meter
    #_:noscript #_:object #_:output #_:picture
    #_:progress :q #_:ruby :s :samp :script
    #_:select #_:slot :small :span :strong
    :sub :sup #_:svg #_:template :time :u
    :tt :var #_:video :wbr})

(def metadata-tags
  "MDN list of metadata content element tags"
  #{#_ :base :link :meta #_:noscript :script
    :style :title})

(def flow-tags
  "MDN list of flow content element tags"
  #{:a :abbr :aside :address :article #_:audio :b :bdo :bdi
    :blockquote :br #_:button #_:canvas :cite
    :code :data #_:datalist :del :details :dfn
    :div :dl :em #_:embed #_:fieldset :figure
    :footer #_:form :h1 :h2 :h3 :h4 :h5 :h6
    :header :hr :i #_:iframe :img #_:input :ins
    :kbd #_:label :link :main #_:map :mark #_:math #_:menu
    #_:meter :nav #_:noscript #_:object :ol #_:output
    :p #_:picture :pre #_:progress :q #_:ruby :s
    :samp :script :section #_:select :small
    :span :strong :sub :sup #_:svg :table
    #_:template #_:textarea :time :ul :var #_:video
    #_:wbr})

(def sectioning-tags
  "MDN list of sectioning content element tags"
  #{:article :aside :nav :section})

(def heading-tags
  "MDN list of heading content element tags"
  #{:h1 :h2 :h3 :h4 :h5 :h6 :hgroup})

(def phrasing-tags
  "MDN list of phrasing content element tags"
  #{:abbr #_:audio :b #_ :bdi :bdo :br #_:button #_:canvas :cite
    :code :data #_:datalist :dfn #_ :del :em #_:embed :i #_:iframe :img
    #_:input :kbd #_:label :mark #_:math #_:meter #_:noscript
    #_:object #_:output #_:picture #_:progress :q #_:ruby :s :samp
    :script #_:select :small :span :strong :sub :sup #_:svg
    #_:textarea :time :var #_:video #_ :wbr})

(def phrasing-subtags
  "MDN list of tags that are phrasing content when they contain only phrasing content."
  #{:a #_ :area :del :ins :link :map #_ :meta})

(def embedded-tags
  "MDN list of embedded content element tags"
  #{:audio :canvas #_:embed :iframe :img #_:math
    :object #_:picture :svg :video})

(def interactive-tags
  "MDN list of interactive content element tags"
  #{:a #_:button :details #_:embed #_:iframe #_:label
    #_:select #_:textarea})

(def transparent-tags
  "MDN list of transparent content tags"
  #{:ins :del :object})

(def external-link-pattern (re-pattern "https?://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]"))
(def internal-link-pattern (re-pattern "/[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]"))

(def url
  [:orn
   [:external [:re external-link-pattern]]
   [:internal [:re internal-link-pattern]]])

(def global-attributes
  "MDN list of global HTML attributes as malli schema"
  (mu/optional-keys
   [:map
    [:class :string]
    [:contenteditable [:enum "true" "false" ""]]
    [:dir [:enum "ltr" "rtl" "auto"]]
    [:hidden :boolean]
    [:id :string]
    [:itemid :string]
    [:itemprop :string]
    [:itemref :string]
    [:itemscope :boolean]
    [:itemtype [:re external-link-pattern]]
    [:lang [:enum "en"]]
    [:tabindex :int]
    [:title :string]
    [:part :string]]))

(def atomic-element
 [:orn
  [:bool :boolean]
  [:decimal :double]
  [:integer :int]
  [:text :string]
  [:nil nil?]])

(defn ->hiccup-schema [tag attr-model content-model]
  (let [head
        [:catn
         [:tag [:= tag]]
         [:attrs (if (schema/has-reqd? attr-model)
                   attr-model
                   [:? attr-model])]]]
    ;; [:and vector?
      (if (nil? content-model)
        head
        (conj head [:contents content-model]))
    ;;  ]
      ))

(defn ns-kw
  ([ns kw] (keyword (str ns) (str (name kw))))
  ([kw] (ns-kw *ns* kw)))

(def html
  [:schema
   {:registry
    {"a-phrasing"
     (->hiccup-schema
      :a
      (mu/merge
       global-attributes
       [:map
        [:href url]
        [:download {:optional true} :string]
        [:rel {:optional true} :string]
        [:target {:optional true}
         [:enum "_self" "_blank" "_parent" "_top"]]])
      [:* [:schema [:ref ::phrasing-content]]])
     "del-phrasing"
     (->hiccup-schema
      :del
      (mu/merge
       global-attributes
       [:map [:cite {:optional true} :string]
        [:datetime {:optional true} :string]])
      [:* [:schema [:ref ::phrasing-content]]])
     "ins-phrasing"
     (->hiccup-schema
      :ins
      (mu/merge
       global-attributes
       [:map [:cite {:optional true} :string]
        [:datetime {:optional true} :string]])
      [:* [:schema [:ref ::phrasing-content]]])
     "link-phrasing"
     (->hiccup-schema
      :ins
      [:altn
       [:main
        (mu/merge
         global-attributes
         [:map
          [:itemprop :string]
          [:crossorigin {:optional true}
           [:enum "anonymous" "use-credentials"]]
          [:href {:optional true} url]
          [:media {:optional true} :string]
          [:rel {:optional true} :string]])]
       [:pre
        (mu/merge
         global-attributes
         [:map
          [:itemprop :string]
          [:crossorigin {:optional true}
           [:enum "anonymous" "use-credentials"]]
          [:href {:optional true} url]
          [:media {:optional true} :string]
          [:rel [:enum "preload" "prefetch"]]
          [:as [:enum "audio" "document" "embed"
                "fetch" "font" "image" "object"
                "script" "style" "track" "video" "worker"]]])]]
      nil)
     ::abbr (->hiccup-schema
             :abbr
             (mu/merge global-attributes
                       [:map [:title :string]])
             [:* [:schema [:ref ::phrasing-content]]])
     #_ ::area
     #_ ::audio
     ::b (->hiccup-schema
          :b
          global-attributes
          [:* [:schema [:ref ::phrasing-content]]])
     ::bdo (->hiccup-schema
            :bdo
            (mu/merge global-attributes
                      [:map [:dir [:enum "ltr" "rtl"]]])
            [:* [:schema [:ref ::phrasing-content]]])
     ::br (->hiccup-schema :br global-attributes nil)
     #_ ::button
     #_ ::canvas
     ::cite (->hiccup-schema
             :cite
             global-attributes
             [:* [:schema [:ref ::phrasing-content]]])
     ::code (->hiccup-schema
             :code
             global-attributes
             [:* [:schema [:ref ::phrasing-content]]])
     ::data (->hiccup-schema
             :data
             (mu/merge global-attributes
                       [:map [:value :string]])
             [:* [:schema [:ref ::phrasing-content]]])
     #_ ::datalist
     ::dfn (->hiccup-schema
            :dfn
            global-attributes
            [:* [:orn [:atomic-element atomic-element]
                 [:node
                  (apply
                   conj
                   [:orn
                    [:a [:schema [:ref "a-phrasing"]]]
                    [:del [:schema [:ref "del-phrasing"]]]
                    [:ins [:schema [:ref "ins-phrasing"]]]
                    [:link [:schema [:ref "link-phrasing"]]]]
                   (map (fn [t] [t [:schema [:ref (ns-kw t)]]])
                        (disj phrasing-tags :dfn)))]]])
     ::em (->hiccup-schema
           :em
           global-attributes
           [:* [:schema [:ref ::phrasing-content]]])
     #_ ::embed
     ::i (->hiccup-schema
          :i
          global-attributes
          [:* [:schema [:ref ::phrasing-content]]])
     #_ ::iframe
     ::img (->hiccup-schema
            :img
            (mu/merge
             global-attributes
             [:map
              [:src url]
              [:alt {:optional true} :string]
              [:sizes {:optional true} :string]
              [:width {:optional true} [:and [:> 0] [:<= 8192]]]
              [:height {:optional true} [:and [:> 0] [:<= 8192]]]
              [:loading {:optional true} [:enum "eager" "lazy"]]
              [:decoding {:optional true} [:enum "sync" "async" "auto"]]
              [:crossorigin {:optional true} [:enum "anonymous" "use-credentials"]]])
            nil)
     #_ ::input
     ::kbd (->hiccup-schema
            :kbd global-attributes
            [:* [:schema [:ref ::phrasing-content]]])
     #_ ::label
     ::link
     [:orn
      [:meta (->hiccup-schema
              :link
              (mu/merge
               global-attributes
               [:map
                [:itemprop {:optional true} :string]
                [:crossorigin {:optional true}
                 [:enum "anonymous" "use-credentials"]]
                [:href {:optional true} url]
                [:media {:optional true} :string]
                [:rel {:optional true} :string]])
              nil)]
      [:phrasing [:schema [:ref "link-phrasing"]]]
      [:flow (->hiccup-schema
              :link
              (mu/merge
               global-attributes
               [:map
                [:itemprop :string]
                [:crossorigin {:optional true}
                 [:enum "anonymous" "use-credentials"]]
                [:href {:optional true} url]
                [:media {:optional true} :string]
                [:rel {:optional true} :string]])
              nil)]]
     #_ ::map
     ::mark (->hiccup-schema
             :mark global-attributes
             [:* [:schema [:ref ::phrasing-content]]])
     ::meta [:orn
             [:flow (->hiccup-schema
                     :meta (mu/merge global-attributes
                                     [:map [:itemprop :string]])
                     nil)]
             [:meta (->hiccup-schema :meta global-attributes nil)]]
     #_ ::meter
     #_ ::noscript
     #_ ::object
     #_ ::output
     #_ ::picture
     #_ ::progress
     ::q (->hiccup-schema
          :q (mu/merge global-attributes
                       [:map [:cite {:optional true} :string]])
          [:* [:schema [:ref ::phrasing-content]]])
     #_ ::ruby
     ::s (->hiccup-schema :s global-attributes
                          [:* [:schema [:ref ::phrasing-content]]])
     ::samp (->hiccup-schema
             :samp global-attributes
             [:* [:schema [:ref ::phrasing-content]]])
     ::script (->hiccup-schema
               :script
               (mu/merge
                global-attributes
                [:map
                 [:async {:optional true} [:enum true "async"]]
                 [:crossorigin {:optional true} :string]
                 [:defer {:optional true} [:= true]]
                 [:integrity {:optional true} :string]
                 [:nomodule {:optional true} :string]
                 [:referrerpolicy {:optional true}
                  [:enum "no-referrer" "no-referrer-when-downgrade"
                   "origin" "origin-when-cross-origin" "same-origin"
                   "strict-origin" "strict-origin-when-cross-origin" ""]]
                 [:src {:optional true} url]
                 [:type :string]])
               [:? :string])
     ::small (->hiccup-schema
              :small global-attributes
              [:* [:schema [:ref ::phrasing-content]]])
     ::span (->hiccup-schema
             :span global-attributes
             [:* [:schema [:ref ::phrasing-content]]])
     ::strong (->hiccup-schema
               :strong global-attributes
               [:* [:schema [:ref ::phrasing-content]]])
     ::sub (->hiccup-schema
            :sub global-attributes
            [:* [:schema [:ref ::phrasing-content]]])
     ::sup (->hiccup-schema
            :sup global-attributes
            [:* [:schema [:ref ::phrasing-content]]])
     #_ ::svg
     #_ ::textarea
     ::time (->hiccup-schema
             :time (mu/merge global-attributes
                             [:map [:datetime :string]])
             [:* [:schema [:ref ::phrasing-content]]])
     ::var (->hiccup-schema
            :var global-attributes
            [:* [:schema [:ref ::phrasing-content]]])
     #_ ::video
     ::phrasing-content
     [:orn [:atomic-element atomic-element]
      [:node
       (apply
        conj
        [:orn
         [:a  [:schema [:ref "a-phrasing"]]]
         [:del [:schema [:ref "del-phrasing"]]]
         [:ins [:schema [:ref "ins-phrasing"]]]
         [:link [:schema [:ref "link-phrasing"]]]]
        (map (fn [t] [t [:schema [:ref (ns-kw t)]]])
             phrasing-tags))]]
     ::hgroup
     (->hiccup-schema
      :hgroup
      global-attributes
      [:+
       [:orn
        [:h1 [:schema [:ref ::h1]]]
        [:h2 [:schema [:ref ::h2]]]
        [:h3 [:schema [:ref ::h3]]]
        [:h4 [:schema [:ref ::h4]]]
        [:h5 [:schema [:ref ::h5]]]
        [:h6 [:schema [:ref ::h6]]]]])
     ::h1 (->hiccup-schema
           :h1 global-attributes
           [:* [:schema [:ref ::phrasing-content]]])
     ::h2 (->hiccup-schema
           :h2 global-attributes
           [:* [:schema [:ref ::phrasing-content]]])
     ::h3 (->hiccup-schema
           :h3 global-attributes
           [:* [:schema [:ref ::phrasing-content]]])
     ::h4 (->hiccup-schema
           :h4 global-attributes
           [:* [:schema [:ref ::phrasing-content]]])
     ::h5 (->hiccup-schema
           :h5 global-attributes
           [:* [:schema [:ref ::phrasing-content]]])
     ::h6 (->hiccup-schema
           :h6 global-attributes
           [:* [:schema [:ref ::phrasing-content]]])
     ::heading-content
     [:orn
      [:hgroup [:schema [:ref ::hgroup]]]
      [:h1 [:schema [:ref ::h1]]]
      [:h2 [:schema [:ref ::h2]]]
      [:h3 [:schema [:ref ::h3]]]
      [:h4 [:schema [:ref ::h4]]]
      [:h5 [:schema [:ref ::h5]]]
      [:h6 [:schema [:ref ::h6]]]]
     ::a
     [:orn
      [:phrasing [:schema [:ref "a-phrasing"]]]
      [:flow
       (->hiccup-schema
        :a
        (mu/merge
         global-attributes
         [:map
          [:href [:orn [:link url]
                  [:fragment :string]]]
          [:download {:optional true} :string]
          [:rel {:optional true} :string]
          [:target {:optional true} [:enum "_self" "_blank" "_parent" "_top"]]])
        [:* [:schema [:ref ::flow-content]]])]]
     ::address (->hiccup-schema
                :address global-attributes
                [:* (apply
                     conj [:orn
                           [:atomic-element atomic-element]
                           [:phrasing-content [:schema [:ref ::phrasing-content]]]]
                     (map (fn [t] [t [:schema [:ref (ns-kw t)]]])
                          (set/difference
                           flow-tags
                           heading-tags
                           phrasing-tags
                           sectioning-tags
                           #{:header :footer :address})))])
     ::article (->hiccup-schema
                :article global-attributes
                [:* [:schema [:ref ::flow-content]]])
     ::aside (->hiccup-schema
              :aside global-attributes
              [:* [:schema [:ref ::flow-content]]])
     ::bdi (->hiccup-schema
            :bdi global-attributes [:* [:schema [:ref ::phrasing-content]]])
     ::blockquote (->hiccup-schema
                   :blockquote
                   (mu/merge global-attributes
                             [:map [:cite {:optional true} :string]])
                   [:* [:schema [:ref ::flow-content]]])
     ::del
     [:orn
      [:phrasing [:schema [:ref "del-phrasing"]]]
      [:flow (->hiccup-schema
              :del global-attributes
              [:* [:schema [:ref ::flow-content]]])]]
     ::details (->hiccup-schema
                :details
                global-attributes
                [:catn
                 [:summary
                  [:schema
                   (->hiccup-schema
                    :summary
                    global-attributes
                    [:orn
                     [:heading [:schema [:ref ::heading-content]]]
                     [:phrasing [:or    ; this seems awkward
                                 [:schema [:ref ::phrasing-content]]
                                 [:* [:schema [:ref ::phrasing-content]]]]]])]]
                 [:contents [:* [:schema [:ref ::flow-content]]]]])
     ::div (->hiccup-schema
            :div global-attributes
            [:* [:schema [:ref ::flow-content]]])
     ::dl
     (->hiccup-schema
      :dl
      global-attributes
      [:*
       [:catn
        [:term
         [:+
          [:schema
           (->hiccup-schema
            :dt
            global-attributes
            (apply conj
                   [:orn
                    [:atomic-element atomic-element]
                    [:phrasing-content [:schema [:ref ::phrasing-content]]]
                    [:heading-content [:schema [:ref ::heading-content]]]]
                   (map (fn [t] [t [:schema [:ref (ns-kw t)]]])
                        (set/difference flow-tags phrasing-tags heading-tags sectioning-tags))))]]]
        [:details
         [:+ [:schema (->hiccup-schema
                       :dd global-attributes
                       [:* [:schema [:ref ::flow-content]]])]]]]])
     ::figure (->hiccup-schema
               :figure global-attributes
               [:altn
                [:caption-first
                 [:catn
                  [:figcaption
                   [:schema
                    (->hiccup-schema
                     :figcaption
                     global-attributes
                     [:* [:schema [:ref ::flow-content]]])]]
                  [:rest [:* [:schema [:ref ::flow-content]]]]]]
                [:caption-last
                 [:catn
                  [:rest [:* [:schema [:ref ::flow-content]]]]
                  [:figcaption
                   [:schema
                    (->hiccup-schema
                     :figcaption
                     global-attributes
                     [:* [:schema [:ref ::flow-content]]])]]]]
                [:no-caption
                 [:* [:schema [:ref ::flow-content]]]]])
     ::footer
     (->hiccup-schema
      :footer global-attributes
      [:* (apply
           conj
           [:orn [:atomic-element atomic-element]
            [:phrasing-content [:schema [:ref ::phrasing-content]]]
            [:heading-content [:schema [:ref ::heading-content]]]]
           (map (fn [t] [t [:schema [:ref (ns-kw t)]]])
                (set/difference flow-tags phrasing-tags heading-tags #{:header :footer})))])
     ::header
     (->hiccup-schema
      :header global-attributes
      [:* (apply
           conj
           [:orn [:atomic-element atomic-element]
            [:phrasing-content [:schema [:ref ::phrasing-content]]]
            [:heading-content [:schema [:ref ::heading-content]]]]
           (map (fn [t] [t [:schema [:ref (ns-kw t)]]])
                (set/difference flow-tags phrasing-tags heading-tags #{:header :footer})))])
     ::hr (->hiccup-schema :hr global-attributes nil)
     ::ins
     [:orn
      [:phrasing [:schema [:ref "ins-phrasing"]]]
      [:flow (->hiccup-schema
              :ins global-attributes
              [:* [:schema [:ref ::flow-content]]])]]
     ::main (->hiccup-schema
             :main global-attributes
             [:* [:schema [:ref ::flow-content]]])
     ::nav (->hiccup-schema :nav global-attributes
                            [:* [:schema [:ref ::flow-content]]])
     ::ol (->hiccup-schema
           :ol
           (mu/merge
            global-attributes
            [:map
             [:reversed {:optional true} :boolean]
             [:start {:optional true} [:and [:> 0] [:< 65536]]]
             [:type {:optional true} [:enum "a" "A" "i" "I" "1"]]])
           [:*
            [:orn
             [:li (->hiccup-schema
                   :li
                   (mu/merge
                    global-attributes
                    [:map [:value {:optional true} :int]])
                   [:* [:schema [:ref ::flow-content]]])]
             [:script [:schema [:ref ::script]]]]])
     ::p (->hiccup-schema
          :p global-attributes
          [:* [:schema [:ref ::phrasing-content]]])
     ::pre (->hiccup-schema :pre global-attributes
                            [:* [:schema [:ref ::phrasing-content]]])
     ::section (->hiccup-schema :section global-attributes
                                [:* [:schema [:ref ::flow-content]]])
     ::table [:schema
              {:registry
               {::caption (->hiccup-schema
                           :caption
                           global-attributes
                           [:* [:schema [:ref ::flow-content]]])
                ::col (->hiccup-schema
                       :col
                       (mu/merge global-attributes
                                 [:map [:span {:optional true} [:>= 1]]])
                       nil)
                ::colgroup
                [:orn
                 [:empty-span
                  (->hiccup-schema
                   :colgroup
                   (mu/merge global-attributes
                             [:map [:span [:>= 1]]])
                   nil)]
                 [:cols (->hiccup-schema
                         :colgroup
                         (mu/merge global-attributes
                                   [:map [:span {:optional true} [:>= 1]]])
                         [:* [:schema [:ref ::col]]])]]
                ::th (->hiccup-schema
                      :th
                      (mu/merge global-attributes
                                [:map
                                 [:abbr {:optional true} :string]
                                 [:colspan {:optional true} [:and [:> 0] [:< 65534]]]
                                 [:rowspan {:optional true} [:and [:> 0] [:< 65534]]]
                                 [:headers {:optional true} :string]
                                 [:scope {:optional true} [:enum "row" "col" "rowgroup"
                                                           "colgroup" "auto"]]])
                      [:*
                       (apply conj
                              [:orn [:atomic-element atomic-element]
                               [:phrasing-content [:schema [:ref ::phrasing-content]]]
                               [:heading-content [:schema [:ref ::heading-content]]]]
                              (map (fn [t] [t [:schema [:ref (ns-kw t)]]])
                                   (set/difference flow-tags
                                                   phrasing-tags
                                                   sectioning-tags
                                                   heading-tags #{:table :footer :header})))])
                ::td (->hiccup-schema
                      :td
                      (mu/merge
                       global-attributes
                       [:map
                        [:colspan {:optional true} [:and [:> 0] [:< 65534]]]
                        [:rowspan {:optional true} [:and [:> 0] [:< 65534]]]
                        [:headers {:optional true} :string]])
                      [:* [:schema [:ref ::flow-content]]])
                ::thead (->hiccup-schema
                         :thead
                         global-attributes
                         [:* [:schema [:ref ::tr]]])
                ::tbody (->hiccup-schema
                         :tbody
                         global-attributes
                         [:* [:schema [:ref ::tr]]])
                ::tfoot (->hiccup-schema
                         :tfoot
                         global-attributes
                         [:* [:schema [:ref ::tr]]])
                ::tr (->hiccup-schema
                      :tr
                      global-attributes
                      [:*
                       [:orn [:th [:schema [:ref ::th]]]
                        [:td [:schema [:ref ::td]]]]])
                ::table
                (->hiccup-schema
                 :table
                 global-attributes
                 [:catn
                  [:caption [:? [:schema [:ref ::caption]]]]
                  [:colgroups [:* [:schema [:ref ::colgroup]]]]
                  [:header [:? [:schema [:ref ::thead]]]]
                  [:contents
                   [:altn
                    [:body
                     [:* [:schema [:ref ::tbody]]]]
                    [:rows [:+ [:schema [:ref ::tr]]]]]]
                  [:footer [:? [:schema [:ref ::tfoot]]]]])}}
              ::table]
     ::ul (->hiccup-schema
           :ul global-attributes
           [:* [:orn
                [:li (->hiccup-schema
                      :li global-attributes
                      [:* [:schema [:ref ::flow-content]]])]
                [:script [:schema [:ref ::script]]]]])
     ::flow-content
     (apply conj
            [:orn
             [:atomic-element atomic-element]
             [:phrasing [:schema [:ref ::phrasing-content]]]
             [:heading  [:schema [:ref ::heading-content]]]]
            (map (fn [t] [t [:schema [:ref (ns-kw t)]]])
                 (set/difference
                  flow-tags phrasing-tags heading-tags)))
     #_ ::base #_ []
     ::metadata-content
     [:orn
      [:style [:schema (->hiccup-schema
                        :style
                        (mu/merge global-attributes
                                  [:map [:media {:optional true} :string]])
                        :string)]]
      [:title [:schema (->hiccup-schema :title global-attributes :string)]]
      [:script [:schema [:ref ::script]]]
      [:meta [:schema [:ref ::meta]]]
      [:link [:schema [:ref ::link]]]]
     ::head (->hiccup-schema
             :head
             global-attributes
             [:* [:schema [:ref ::metadata-content]]])
     ::body (->hiccup-schema
             :body
             global-attributes
             [:* [:schema [:ref ::flow-content]]])
     ::html (->hiccup-schema
             :html
             global-attributes
             [:catn [:head [:schema [:ref ::head]]]
              [:body [:schema [:ref ::body]]]])
     ::element
     [:orn
      [:flow [:schema [:ref ::flow-content]]]
      [:heading [:schema [:ref ::heading-content]]]
      [:phrasing [:schema [:ref ::phrasing-content]]]]}}
   ::html])

(def element? (m/validator (schema/subschema html ::element)))

(def element-validators
  (let [kws (filter keyword? (keys (get (second html) :registry)))]
    (into {:atomic-element (m/validator atomic-element)}
          (map (fn [t] [t (m/validator (schema/subschema html (ns-kw t)))])
               kws))))

(def element-explainers
  (let [kws (filter keyword? (keys (get (second html) :registry)))]
    (into {:atomic-element (m/explainer atomic-element)}
          (map (fn [t] [t (m/explainer (schema/subschema html (ns-kw t)))])
               kws))))

;; "content is palpable when it's neither empty or hidden;
;; it is content that is rendered and is substantive.
;; Elements whose model is flow content or phrasing content
;; should have at least one node which is palpable."
(defn palpable? [c]
  (some? (some
          #(m/validate atomic-element %)
          (tree-seq #(and (vector? %) (keyword? (first %))) rest c))))

(def phrasing? (m/validator (schema/subschema html ::phrasing-content)))

(defn validate-element [elem]
  (if (and (vector? elem)
           (keyword? (first elem)))
    (let [form-kw (first elem)
          valid? (get element-validators (ns-kw form-kw))]
      (if (valid? elem)
                {:result elem}
                {:err {:type ::invalid-form
                       :message (str "Invalid <" (name form-kw) "> form")
                       :cause ((get element-explainers (ns-kw form-kw)) elem)}}))
    elem))
