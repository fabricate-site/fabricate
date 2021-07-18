(ns site.fabricate.sketch
  "Namespace for mapping out concepts that are not yet prototypes.
  Highly experimental, subject to arbitrary changes."
  (:require
   [malli.core :as m]
   [malli.error :as me]
   [malli.util :as mu]
   [hiccup2.core :as hiccup]
   [site.fabricate.prototype.read :as read]
   [site.fabricate.prototype.html :as html]
   [site.fabricate.prototype.page :as page]
   [site.fabricate.prototype.schema :as schema]))

(defn file? [f] (instance? java.io.File f))

(def page-metadata-schema
  [:map
   [:input-file [:orn [:path :string]
                 [:file [:fn file?]]]]
   [:unparsed-content {:optional true} :string]
   [:parsed-content {:optional true} [:fn vector?]]
   [:title {:optional true} :string]
   [:namespace {:optional true}
    [:orn
     [:name symbol?]
     [:form [:fn schema/ns-form?]]]]
   [:page-style {:optional true} :string]
   [:output-file {:optional true}
    [:orn [:undefined nil?]
     [:path :string]
     [:file [:fn file?]]]]
   [:hiccup-content {:optional true} html/html]
   [:rendered-content {:optional true} :string]])

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
               :namespace 'demo-ns}))

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

(comment
  ;; schemas all the way down: schemas are states are data
  (def malli-fsm-cyclic
    {[:enum :a] [:or [:enum :b] [:enum :c]]
     [:enum :b] [:or [:enum :d]]
     [:enum :c] [:or [:enum :d]]
     [:enum :d] [:or [:enum :a]]})

  (def malli-fsm-dag
    {:fsm/start [:enum :a]
     [:enum :a] [:or [:enum :b] [:enum :c]]
     [:enum :b] [:or [:enum :d]]
     [:enum :c] [:or [:enum :d]]
     [:enum :d] [:enum :fsm/end]})



  (def  function-state-machine
    "Function state machine: pattern guards are defined solely by
    the contract/schema/signature of the function's inputs and outputs."
    {:init 'slurp
     'slurp 'read/parse
     'read/parse 'read/eval-with-errors
     'read/eval-with-errors 'template->html
     'template->html 'spit
     'spit :exit}))





(comment (assert (= 4 (advance-finite-schema-machine {:int inc} 3))))



(comment (defn >5? [x] (if (> 5 x) :greater :lesser))

         (def malli-fsm-data-fns
           "FSM: Finite Schema Machine"
           {:string {:op identity
                     :exit-state :fsm/end}
            :int {:op >5?
                  :exit-state :checked}
            [:enum :greater] {:op (fn [_] "greater")
                              :exit-state :fsm/end}
            [:enum :lesser] {:op (fn [_] "lesser")
                             :exit-state :fsm/end}})



         (->> 3
              (advance-malli-fsm malli-fsm-data-fns)
              (advance-malli-fsm malli-fsm-data-fns)
              (advance-malli-fsm malli-fsm-data-fns)
              ))





;; A vector of state-action tuples could be "compiled" and
;; utility functions like m/parser could be used to derive
;; functions by which arbitrary values could be quickly matched
;; against all states for dispatch

(comment
  (m/validate [:or [:map {:closed true
                          :description "map1"}
                    [:a :string]]
               [:map {:description "map2"}
                [:a :string]
                [:b {:optional true} :string]]]
              {:a "a"})
  (m/parse [:orn [:map1 [:map {:closed true
                               :description "map1"}
                         [:a :string]]]
            [:map2 [:map {:description "map2"}
                    [:a :string]
                    [:b {:optional true} :string]]]]
           {:a "a"})

  (m/parse [:orn
            [:map2 [:map {:description "map2"}
                    [:a :string]
                    [:b {:optional true} :string]]]

            [:map1 [:map {:closed true
                          :description "map1"}
                    [:a :string]]]
            ]
           {:a "a"}))

(comment
  (defn maybe-inc
    ([value prob]
     (let [p (rand 1)]
       (if (>= p prob) value (inc value))))
    ([prob] (fn [value] (maybe-inc value prob)))
    ([] (maybe-inc 0.95)))

  ;; if a probabilistic function is passed in to the
  ;; state advancement mechanism, you get a Markov chain?

  (let [num-seq (iterate (maybe-inc 0.65) 1)]
    (reduce (fn [num n] (if (= n num) (reduced num) n))
            num-seq))

  )
