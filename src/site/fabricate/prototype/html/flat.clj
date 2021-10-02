(ns site.fabricate.prototype.html.flat
  "Flattened version of the HTML schemas"
  (:require
   [clojure.string :as str]
   [clojure.set :as set]
   [clojure.test.check.generators :as gen]
   [com.gfredericks.test.chuck.generators :as gen']
   [malli.core :as m]
   [malli.generator :as mg]
   [malli.registry :as mr]
   [malli.dot :as md]
   [malli.error :as me]
   [malli.util :as mu]
   [site.fabricate.prototype.schema :as schema]
   [site.fabricate.prototype.html :as html]))

(def atomic-element
  [:or :boolean :double
   :int :string :nil])

(defn ->hiccup-schema [tag attr-model content-model]
  (let [head
        [:catn
         [:tag [:= tag]]
         [:attr (if (schema/has-reqd? attr-model)
                  attr-model
                  [:? attr-model])]]]
    (if (nil? content-model)
      head
      (conj head [:contents content-model]))))

(def url
  [:or [:re html/external-link-pattern]
   [:re html/internal-link-pattern]])

(def html
  [:schema
   {:registry
    {"a-phrasing"
     (->hiccup-schema
      :a
      (mu/merge
       html/global-attributes
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
       html/global-attributes
       [:map [:cite {:optional true} :string]
        [:datetime {:optional true} :string]])
      [:* [:schema [:ref ::phrasing-content]]])
     "ins-phrasing"
     (->hiccup-schema
      :ins
      (mu/merge
       html/global-attributes
       [:map [:cite {:optional true} :string]
        [:datetime {:optional true} :string]])
      [:* [:schema [:ref ::phrasing-content]]])
     "link-phrasing"
     (->hiccup-schema
      :ins
      [:alt
       (mu/merge
        html/global-attributes
        [:map
         [:itemprop :string]
         [:crossorigin {:optional true}
          [:enum "anonymous" "use-credentials"]]
         [:href {:optional true} url]
         [:media {:optional true} :string]
         [:rel {:optional true} :string]])
       (mu/merge
        html/global-attributes
        [:map
         [:itemprop :string]
         [:crossorigin {:optional true}
          [:enum "anonymous" "use-credentials"]]
         [:href {:optional true} url]
         [:media {:optional true} :string]
         [:rel [:enum "preload" "prefetch"]]
         [:as [:enum "audio" "document" "embed"
               "fetch" "font" "image" "object"
               "script" "style" "track" "video" "worker"]]])]
      nil)
     ::abbr (->hiccup-schema
             :abbr
             (mu/merge html/global-attributes
                       [:map [:title :string]])
             [:* [:schema [:ref ::phrasing-content]]])
     #_::area
     #_::audio
     ::b (->hiccup-schema
          :b
          html/global-attributes
          [:* [:schema [:ref ::phrasing-content]]])
     ::bdo (->hiccup-schema
            :bdo
            (mu/merge html/global-attributes
                      [:map [:dir [:enum "ltr" "rtl"]]])
            [:* [:schema [:ref ::phrasing-content]]])
     ::br (->hiccup-schema :br html/global-attributes nil)
     #_::button
     #_::canvas
     ::cite (->hiccup-schema
             :cite
             html/global-attributes
             [:* [:schema [:ref ::phrasing-content]]])
     ::code (->hiccup-schema
             :code
             html/global-attributes
             [:* [:schema [:ref ::phrasing-content]]])
     ::data (->hiccup-schema
             :data
             (mu/merge html/global-attributes
                       [:map [:value :string]])
             [:* [:schema [:ref ::phrasing-content]]])
     #_::datalist
     ::dfn (->hiccup-schema
            :dfn
            html/global-attributes
            [:*
             (apply conj
                    [:or  atomic-element
                     [:schema [:ref "a-phrasing"]]
                     [:schema [:ref "del-phrasing"]]
                     [:schema [:ref "ins-phrasing"]]
                     [:schema [:ref "link-phrasing"]]]
                    (map (fn [t] [:schema [:ref (html/ns-kw t)]])
                         (disj html/phrasing-tags :dfn)))])
     ::em (->hiccup-schema
           :em
           html/global-attributes
           [:* [:schema [:ref ::phrasing-content]]])
     #_::embed
     ::i (->hiccup-schema
          :i
          html/global-attributes
          [:* [:schema [:ref ::phrasing-content]]])
     #_::iframe
     ::img (->hiccup-schema
            :img
            (mu/merge
             html/global-attributes
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
     #_::input
     ::kbd (->hiccup-schema
            :kbd html/global-attributes
            [:* [:schema [:ref ::phrasing-content]]])
     #_::label
     ::link
     [:or
      (->hiccup-schema
       :link
       (mu/merge
        html/global-attributes
        [:map
         [:itemprop {:optional true} :string]
         [:crossorigin {:optional true}
          [:enum "anonymous" "use-credentials"]]
         [:href {:optional true} url]
         [:media {:optional true} :string]
         [:rel {:optional true} :string]])
       nil)
      [:schema [:ref "link-phrasing"]]
      (->hiccup-schema
       :link
       (mu/merge
        html/global-attributes
        [:map
         [:itemprop :string]
         [:crossorigin {:optional true}
          [:enum "anonymous" "use-credentials"]]
         [:href {:optional true} url]
         [:media {:optional true} :string]
         [:rel {:optional true} :string]])
       nil)]
     #_::map
     ::mark (->hiccup-schema
             :mark html/global-attributes
             [:* [:schema [:ref ::phrasing-content]]])
     ::meta [:or
             (->hiccup-schema
              :meta (mu/merge html/global-attributes
                              [:map [:itemprop :string]])
              nil)
             (->hiccup-schema :meta html/global-attributes nil)]
     #_::meter
     #_::noscript
     #_::object
     #_::output
     #_::picture
     #_::progress
     ::q (->hiccup-schema
          :q (mu/merge html/global-attributes
                       [:map [:cite {:optional true} :string]])
          [:* [:schema [:ref ::phrasing-content]]])
     #_::ruby
     ::s (->hiccup-schema :s html/global-attributes
                          [:* [:schema [:ref ::phrasing-content]]])
     ::samp (->hiccup-schema
             :samp html/global-attributes
             [:* [:schema [:ref ::phrasing-content]]])
     ::script (->hiccup-schema
               :script
               (mu/merge
                html/global-attributes
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
              :small html/global-attributes
              [:* [:schema [:ref ::phrasing-content]]])
     ::span (->hiccup-schema
             :span html/global-attributes
             [:* [:schema [:ref ::phrasing-content]]])
     ::strong (->hiccup-schema
               :strong html/global-attributes
               [:* [:schema [:ref ::phrasing-content]]])
     ::sub (->hiccup-schema
            :sub html/global-attributes
            [:* [:schema [:ref ::phrasing-content]]])
     ::sup (->hiccup-schema
            :sup html/global-attributes
            [:* [:schema [:ref ::phrasing-content]]])
     #_::svg
     #_::textarea
     ::time (->hiccup-schema
             :time (mu/merge html/global-attributes
                             [:map [:datetime :string]])
             [:* [:schema [:ref ::phrasing-content]]])
     ::var (->hiccup-schema
            :var html/global-attributes
            [:* [:schema [:ref ::phrasing-content]]])
     #_::video
     ::phrasing-content
     [:or  atomic-element
      (apply
       conj
       [:or
        [:schema [:ref "a-phrasing"]]
        [:schema [:ref "del-phrasing"]]
        [:schema [:ref "ins-phrasing"]]
        [:schema [:ref "link-phrasing"]]]
       (map (fn [t] [:schema [:ref (html/ns-kw t)]])
            html/phrasing-tags))]
     ::hgroup
     (->hiccup-schema
      :hgroup
      html/global-attributes
      [:+
       [:orn
        [:h1 [:schema [:ref ::h1]]]
        [:h2 [:schema [:ref ::h2]]]
        [:h3 [:schema [:ref ::h3]]]
        [:h4 [:schema [:ref ::h4]]]
        [:h5 [:schema [:ref ::h5]]]
        [:h6 [:schema [:ref ::h6]]]]])
     ::h1 (->hiccup-schema
           :h1 html/global-attributes
           [:* [:schema [:ref ::phrasing-content]]])
     ::h2 (->hiccup-schema
           :h2 html/global-attributes
           [:* [:schema [:ref ::phrasing-content]]])
     ::h3 (->hiccup-schema
           :h3 html/global-attributes
           [:* [:schema [:ref ::phrasing-content]]])
     ::h4 (->hiccup-schema
           :h4 html/global-attributes
           [:* [:schema [:ref ::phrasing-content]]])
     ::h5 (->hiccup-schema
           :h5 html/global-attributes
           [:* [:schema [:ref ::phrasing-content]]])
     ::h6 (->hiccup-schema
           :h6 html/global-attributes
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
         html/global-attributes
         [:map
          [:href [:orn [:link url]
                  [:fragment :string]]]
          [:download {:optional true} :string]
          [:rel {:optional true} :string]
          [:target {:optional true} [:enum "_self" "_blank" "_parent" "_top"]]])
        [:* [:schema [:ref ::flow-content]]])]]
     ::address (->hiccup-schema
                :address html/global-attributes
                [:* (apply
                     conj [:orn
                           [:atomic-element atomic-element]
                           [:phrasing-content [:schema [:ref ::phrasing-content]]]]
                     (map (fn [t] [t [:schema [:ref (html/ns-kw t)]]])
                          (set/difference
                           html/flow-tags
                           html/heading-tags
                           html/phrasing-tags
                           html/sectioning-tags
                           #{:header :footer :address})))])
     ::article (->hiccup-schema
                :article html/global-attributes
                [:* [:schema [:ref ::flow-content]]])
     ::aside (->hiccup-schema
              :aside html/global-attributes
              [:* [:schema [:ref ::flow-content]]])
     ::bdi (->hiccup-schema
            :bdi html/global-attributes [:* [:schema [:ref ::phrasing-content]]])
     ::blockquote (->hiccup-schema
                   :blockquote
                   (mu/merge html/global-attributes
                             [:map [:cite {:optional true} :string]])
                   [:* [:schema [:ref ::flow-content]]])
     ::del
     [:or
      [:schema [:ref "del-phrasing"]]
      (->hiccup-schema
       :del html/global-attributes
       [:* [:schema [:ref ::flow-content]]])]
     ::details (->hiccup-schema
                :details
                html/global-attributes
                [:cat

                 [:schema
                  (->hiccup-schema
                   :summary
                   html/global-attributes
                   [:or
                    [:schema [:ref ::heading-content]]
                    [:or              ; this seems awkward
                     [:schema [:ref ::phrasing-content]]
                     [:* [:schema [:ref ::phrasing-content]]]]])]
                 [:* [:schema [:ref ::flow-content]]]])
     ::div (->hiccup-schema
            :div html/global-attributes
            [:* [:schema [:ref ::flow-content]]])
     ::dl
     (->hiccup-schema
      :dl
      html/global-attributes
      [:repeat
       [:cat
        [:schema
         (->hiccup-schema
          :dt
          html/global-attributes
          (apply conj
                 [:or
                  atomic-element
                  [:ref ::phrasing-content]
                  [:ref ::heading-content]]
                 (map (fn [t] [:schema [:ref (html/ns-kw t)]])
                      (set/difference html/flow-tags html/phrasing-tags html/heading-tags html/sectioning-tags))))]
        [:schema (->hiccup-schema
                  :dd html/global-attributes
                  [:* [:schema [:ref ::flow-content]]])]]])
     ::figure (->hiccup-schema
               :figure html/global-attributes
               [:alt
                [:catn
                 [:figcaption
                  [:schema
                   (->hiccup-schema
                    :figcaption
                    html/global-attributes
                    [:* [:schema [:ref ::flow-content]]])]]
                 [:rest [:* [:schema [:ref ::flow-content]]]]]
                [:catn
                 [:rest [:* [:schema [:ref ::flow-content]]]]
                 [:figcaption
                  [:schema
                   (->hiccup-schema
                    :figcaption
                    html/global-attributes
                    [:* [:schema [:ref ::flow-content]]])]]]
                [:* [:schema [:ref ::flow-content]]]])
     ::footer
     (->hiccup-schema
      :footer html/global-attributes
      [:* (apply
           conj
           [:or  atomic-element
            [:schema [:ref ::phrasing-content]]
            [:schema [:ref ::heading-content]]]
           (map (fn [t] [:schema [:ref (html/ns-kw t)]])
                (set/difference html/flow-tags html/phrasing-tags html/heading-tags #{:header :footer})))])
     ::header
     (->hiccup-schema
      :header html/global-attributes
      [:* (apply
           conj
           [:or atomic-element
            [:schema [:ref ::phrasing-content]]
            [:schema [:ref ::heading-content]]]
           (map (fn [t] [:schema [:ref (html/ns-kw t)]])
                (set/difference html/flow-tags html/phrasing-tags html/heading-tags #{:header :footer})))])
     ::hr (->hiccup-schema :hr html/global-attributes nil)
     ::ins
     [:or
      [:schema [:ref "ins-phrasing"]]
      (->hiccup-schema
       :ins html/global-attributes
       [:* [:schema [:ref ::flow-content]]])]
     ::main (->hiccup-schema
             :main html/global-attributes
             [:* [:schema [:ref ::flow-content]]])
     ::nav (->hiccup-schema :nav html/global-attributes
                            [:* [:schema [:ref ::flow-content]]])
     ::ol (->hiccup-schema
           :ol
           (mu/merge
            html/global-attributes
            [:map
             [:reversed {:optional true} :boolean]
             [:start {:optional true} [:and [:> 0] [:< 65536]]]
             [:type {:optional true} [:enum "a" "A" "i" "I" "1"]]])
           [:*
            [:or
             (->hiccup-schema
              :li
              (mu/merge
               html/global-attributes
               [:map [:value {:optional true} :int]])
              [:* [:schema [:ref ::flow-content]]])
             [:schema [:ref ::script]]]])
     ::p (->hiccup-schema
          :p html/global-attributes
          [:* [:schema [:ref ::phrasing-content]]])
     ::pre (->hiccup-schema :pre html/global-attributes
                            [:* [:schema [:ref ::phrasing-content]]])
     ::section (->hiccup-schema :section html/global-attributes
                                [:* [:schema [:ref ::flow-content]]])
     ::table [:schema
              {:registry
               {::caption (->hiccup-schema
                           :caption
                           html/global-attributes
                           [:* [:schema [:ref ::flow-content]]])
                ::col (->hiccup-schema
                       :col
                       (mu/merge html/global-attributes
                                 [:map [:span {:optional true} [:>= 1]]])
                       nil)
                ::colgroup
                [:or
                 (->hiccup-schema
                  :colgroup
                  (mu/merge html/global-attributes
                            [:map [:span [:>= 1]]])
                  nil)
                 (->hiccup-schema
                  :colgroup
                  (mu/merge html/global-attributes
                            [:map [:span {:optional true} [:>= 1]]])
                  [:* [:schema [:ref ::col]]])]
                ::th (->hiccup-schema
                      :th
                      (mu/merge html/global-attributes
                                [:map
                                 [:abbr {:optional true} :string]
                                 [:colspan {:optional true} [:and [:> 0] [:< 65534]]]
                                 [:rowspan {:optional true} [:and [:> 0] [:< 65534]]]
                                 [:headers {:optional true} :string]
                                 [:scope {:optional true} [:enum "row" "col" "rowgroup"
                                                           "colgroup" "auto"]]])
                      [:*
                       (apply conj
                              [:or  atomic-element
                               [:schema [:ref ::phrasing-content]]
                               [:schema [:ref ::heading-content]]]
                              (map (fn [t] [:schema [:ref (html/ns-kw t)]])
                                   (set/difference html/flow-tags
                                                   html/phrasing-tags
                                                   html/sectioning-tags
                                                   html/heading-tags #{:table :footer :header})))])
                ::td (->hiccup-schema
                      :td
                      (mu/merge
                       html/global-attributes
                       [:map
                        [:colspan {:optional true} [:and [:> 0] [:< 65534]]]
                        [:rowspan {:optional true} [:and [:> 0] [:< 65534]]]
                        [:headers {:optional true} :string]])
                      [:* [:schema [:ref ::flow-content]]])
                ::thead (->hiccup-schema
                         :thead
                         html/global-attributes
                         [:* [:schema [:ref ::tr]]])
                ::tbody (->hiccup-schema
                         :tbody
                         html/global-attributes
                         [:* [:schema [:ref ::tr]]])
                ::tfoot (->hiccup-schema
                         :tfoot
                         html/global-attributes
                         [:* [:schema [:ref ::tr]]])
                ::tr (->hiccup-schema
                      :tr
                      html/global-attributes
                      [:*
                       [:or [:schema [:ref ::th]]
                        [:schema [:ref ::td]]]])
                ::table
                (->hiccup-schema
                 :table
                 html/global-attributes
                 [:cat
                  [:? [:schema [:ref ::caption]]]
                  [:* [:schema [:ref ::colgroup]]]
                  [:? [:schema [:ref ::thead]]]
                  [:alt

                   [:* [:schema [:ref ::tbody]]]
                   [:+ [:schema [:ref ::tr]]]]
                  [:? [:schema [:ref ::tfoot]]]])}}
              ::table]
     ::ul (->hiccup-schema
           :ul html/global-attributes
           [:* [:or
                (->hiccup-schema
                 :li html/global-attributes
                 [:* [:schema [:ref ::flow-content]]])
                [:schema [:ref ::script]]]])
     ::flow-content
     (apply conj
            [:or
             atomic-element
             [:schema [:ref ::phrasing-content]]
             [:schema [:ref ::heading-content]]]
            (map (fn [t] [:schema [:ref (html/ns-kw t)]])
                 (set/difference
                  html/flow-tags html/phrasing-tags html/heading-tags)))
     #_::base #_[]
     ::metadata-content
     [:or
      [:schema (->hiccup-schema
                :style
                (mu/merge html/global-attributes
                          [:map [:media {:optional true} :string]])
                :string)]
      [:schema (->hiccup-schema :title html/global-attributes :string)]
      [:schema [:ref ::script]]
      [:schema [:ref ::meta]]
      [:schema [:ref ::link]]]
     ::head (->hiccup-schema
             :head
             html/global-attributes
             [:* [:schema [:ref ::metadata-content]]])
     ::body (->hiccup-schema
             :body
             html/global-attributes
             [:* [:schema [:ref ::flow-content]]])
     ::html (->hiccup-schema
             :html
             html/global-attributes
             [:cat  [:schema [:ref ::head]]
              [:schema [:ref ::body]]])
     ::element
     [:or
      [:schema [:ref ::flow-content]]
      [:schema [:ref ::heading-content]]
      [:schema [:ref ::phrasing-content]]]}}
   ::html])

(def element (m/schema (schema/subschema html ::element)))
(def element? (m/validator element))
(def parse-element (m/parser element))
(def explain-element (m/explainer element))

(def element-validators
  (let [kws (filter keyword? (keys (get (second html) :registry)))]
    (into {:atomic-element (m/validator atomic-element)}
          (map (fn [t] [t (m/validator (schema/subschema html (html/ns-kw t)))])
               kws))))

(def element-explainers
  (let [kws (filter keyword? (keys (get (second html) :registry)))]
    (into {:atomic-element (m/explainer atomic-element)}
          (map (fn [t] [t (m/explainer (schema/subschema html (html/ns-kw t)))])
               kws))))

(def element-parsers
  (let [kws (filter keyword? (keys (get (second html) :registry)))]
    (into {:atomic-element (m/parser atomic-element)}
          (map (fn [t] [t (m/parser (schema/subschema html (html/ns-kw t)))])
               kws))))

(comment
  (parse-element [:p "text" [:em "with emphasis"]])

  (parse-element [:dl [:dt "term"] [:dd "desc"]]))
