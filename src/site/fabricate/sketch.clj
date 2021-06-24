(ns site.fabricate.sketch
  "Namespace for mapping out concepts that are not yet prototypes.
  Highly experimental, subject to arbitrary changes."
  (:require
   [malli.core :as m]
   [malli.util :as mu]
   [hiccup2.core :as hiccup]
   [site.fabricate.prototype.read :as read]
   [site.fabricate.prototype.html :as html]
   [site.fabricate.prototype.page :as page]
   [site.fabricate.prototype.schema :as schema]))

(def page-metadata-schema
  [:map
   [:title {:optional true} :string]
   [:namespace {:optional true}
    [:orn
     [:name symbol?]
     [:form [:fn schema/ns-form?]]]]
   [:page-style {:optional true} :string]
   [:input-file :string]
   [:output-file {:optional true}
    [:orn [:undefined nil?]
     [:defined :string]]]
   [:page-content
    {:optional true}
    [:orn [:undefined nil?]
     [:unparsed [:cat [:enum :rendered] :string]]
     [:parsed [:fn vector?]]
     [:validated html/html]
     [:rendered [:cat [:enum :rendered] :string]]
     ]]])

(def published-page-metadata-schema
  (mu/required-keys page-metadata-schema
                    [:output-file :namespace :page-content]))

(comment
  (m/validate page-metadata-schema {:title "Example post"
                                    :input-file "./content/example.html.fab"})
  (m/validate published-page-metadata-schema {:title "Example post"})

  (m/validate published-page-metadata-schema
              {:title "Example post"
               :input-file "./content/example.html.fab"
               :output-file "./output/demo-file.html"
               :namespace 'demo-ns})

  )

(def page-schema
  [:map
   ;; optional keys could be used in a state machine
   ;; e.g. when a value for the key is assoc'd into the map,
   ;; perform an action

   [:metadata {:optional true} :string]
   [:html [:orn
           ;; enumerating the cases in a data structure
           ;; rather than in a function
           [:validated html/html]
           [:unvalidated vector?]
           [:rendered string?]]]])

(comment
  ;; function dispatch on the results of m/parse
  (let [renderers {:rendered identity
                   :validated #(hiccup/html %)
                   :unvalidated #(hiccup/html %)}
        {:keys [html]
         :as parsed}
        (m/parse page-schema {:html [:div "text"]})
        [html-status form] html
        content-fn (get renderers html-status)]
    (println html-status)
    (content-fn form))

  ;; assoc-ing new functions into maps like the one above is
  ;; how users could potentially extend fabricate while having
  ;; some guarantees that their functions fit into the structure
  ;; provided by fabricate.

  )

(def live-reload-state-kws
  "State transition map for fabricate's main loop"
  {::watch #{::watch ::file-changed}
   ::file-changed #{::read/parsed
                    ::read/read-error}
   ::read/parsed #{::read/evaluated
                   ::read/eval-error}
   ::read/evaluated #{::page/rendered
                      ::page/render-error}
   ::page/rendered #{::written
                     ::write-error}
   ::written #{::watch}
   ::read/read-error #{::fallback ::skip}
   ::read/eval-error #{::fallback ::skip}
   ::read/render-error #{::fallback ::skip}
   ::write-error #{::fallback ::skip}
   ::fallback #{::written}
   ::skip #{::watch}})

(def live-reload-state-action-behavior
  "State-action behavior map for fabricate's main loop.
  The given function advances the state to the given state."
  {::watch #{['detect-changes ::watch]
             ['detect-changes ::file-changed]}
   ::file-changed #{[read/parse ::read/parsed]
                    [read/parse ::read/read-error]}
   ::read/parsed #{[read/eval-with-errors ::read/evaluated]
                   [read/eval-with-errors ::read/eval-error]}
   ::read/evaluated #{['template->html ::page/rendered]
                      ['template->html ::page/render-error]}
   ::page/rendered #{[spit ::written]
                     [spit ::write-error]}
   ::written #{[identity ::watch]}
   ::read/read-error #{['catch-error ::fallback]
                       ['catch-error ::skip]}
   ::read/eval-error #{['catch-error ::fallback]
                       ['catch-error ::skip]}
   ::read/render-error #{['catch-error ::fallback]
                       ['catch-error ::skip]}
   ::write-error #{['catch-error ::fallback]
                   ['catch-error ::skip]}
   ::fallback #{[spit ::written]}
   ::skip #{[identity ::watch]}})

(def rerender-state-machine
  "State-action machine representing the sequence(s) of operations
  to be performed when juxt.dirwatch/watch-dir detects a file change.

  The goal of representing this core sequence of operations as a
  finite state machine is to make the core logic of fabricate
  understandable, enumerate all possible failure modes and the paths
  by which they can be brought back towards stability, and provide
  a programmable application architecture for users of fabricate."
  {:site.fabricate.prototype.write/file-changed #{[read/parse ::read/parsed]
                          [read/parse ::read/read-error]}
   ::read/parsed #{[read/eval-with-errors ::read/evaluated]
                   [read/eval-with-errors ::read/eval-error]}
   ::read/evaluated #{['template->html ::page/rendered]
                      ['template->html ::page/render-error]}
   ::page/rendered #{[spit :site.fabricate.prototype.write/written]
                     [spit :site.fabricate.prototype.write/write-error]}
   :site.fabricate.prototype.write/written #{[identity :site.fabricate.prototype.write/done]}
   ::read/read-error #{['catch-error :site.fabricate.prototype.write/fallback]
                       ['catch-error :site.fabricate.prototype.write/skip]}
   ::read/eval-error #{['catch-error :site.fabricate.prototype.write/fallback]
                       ['catch-error :site.fabricate.prototype.write/skip]}
   ::read/render-error #{['catch-error :site.fabricate.prototype.write/fallback]
                         ['catch-error :site.fabricate.prototype.write/skip]}
   :site.fabricate.prototype.write/write-error #{['catch-error :site.fabricate.prototype.write/fallback]
                         ['catch-error :site.fabricate.prototype.write/skip]}
   :site.fabricate.prototype.write/fallback #{[spit :site.fabricate.prototype.write/done]}
   :site.fabricate.prototype.write/skip #{[identity :site.fabricate.prototype.write/done]}})


(defn malli?
  "Returns true if the given form is a valid malli schema"
  [form]
  (try (do (m/schema form) true)
       (catch Exception e
           #_(prn (Throwable->map e))
           false)))

(def state-action-behavior
  "Malli schema for the ⟨s,α,t⟩ triple, \"where s and t are
  states and α is an action.\" The action performs the transition
  from s to t.

  In this implementation, states are defined by a malli schema.

  See Lamport [2008] - \"Computation and State Machines\""
  [:catn
   [:start malli?]
   [:action fn?]
   [:end malli?]])
