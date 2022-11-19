(ns site.fabricate.prototype.fsm
  "Namespace for defining finite schema machines. A finite schema
   machine is a method of organizing the functions to be called
   in a program by the state of the data passing through the program.
   Those states are defined using malli schemas.

   It is similar in concept to the idea of a state-action behavior
   in Leslie Lamport's 2008 paper 'Computation and State Machines',
   but the formalism is not as rigorous."
  (:require [malli.core :as m]
            [malli.util :as mu]
            [malli.error :as me]
            [malli.generator :as mg]
            [com.brunobonacci.mulog :as u]
            [site.fabricate.prototype.schema :as schema]))

(def state-action-map
  "Malli schema for the map where each entry is a ⟨s,α⟩ tuple,
  \"where s is a state states and α is an action.\"
  The action performs the transition from s to a target state
  specified by the signature of α.

  In this implementation, states are defined by a malli schema.

  See Lamport [2008] - \"Computation and State Machines\""
  (m/schema [:map-of [:fn schema/malli?] [:fn ifn?]]))

(def fsm-value-map
  "Malli schema for maps defining values with state metadata for finite schema machines."
  (m/schema
   [:map
    [:state/value :any]
    [:state.name/previous {:optional true} [:or :keyword :string [:fn schema/malli?]]]
    [:state.name/matched {:optional true} [:fn schema/malli?]]
    [:state/error {:optional true} :map]]))

(def
  ^{:malli/schema [:=> [:cat :any] :boolean]}
  fsm-value-map?
  "Predicate indicating whether the given value defines a state map for a finite schema machine."
  (m/validator fsm-value-map))

(defn advance
  "Takes a value, matches it against the schema keys defined in the
   input state-action map, and calls the appropriate function to advance
   the value to the next state.

  Additional arguments to the function can be supplied via & args; they will be
  appended to the arguments passed to the matched function via apply.

  Errors in function execution will be printed and the input value will be returned."
  {:malli/schema [:=> [:cat state-action-map
                       [:or fsm-value-map :any]
                       [:* :any]] [:or fsm-value-map :any]]}
  [fsm-map value & args]
  (let [union-schema (schema/unify (keys fsm-map))
        fsm-v-map? (fsm-value-map? value)
        v (if fsm-v-map? (:state/value value) value)
        ;; value (if debug? (:fsm/value value) value)
        parsed (m/parse union-schema v)]
    (u/with-context
        (merge {:state/value v :log/level 300}
               (when fsm-v-map? (select-keys value [:state/name :state/description]))
               (select-keys (meta fsm-map) [:fsm/name :fsm/description]))
      (if (= :malli.core/invalid parsed)
        (do
          (u/log ::unmatched-value
                 :malli/error (me/humanize (m/explain union-schema v)))
          value)
        (let [matched-state (-> union-schema
                                m/children
                                (nth (first parsed))
                                last)
              op (get fsm-map matched-state
                      (get fsm-map (m/form matched-state)
                           (fn [vv]
                             (u/log ::unmatched-value
                                    :malli/error
                                    (me/humanize (m/explain union-schema vv)))
                             vv)))
              result (try (u/trace
                              ::advance
                            (->> (select-keys
                                  (m/properties matched-state)
                                  [:state/name :state/description])
                                 (apply concat)
                                 (into []))
                            (apply op v args))
                          (catch Exception e (Throwable->map e)))
              error-result? (schema/throwable-map? result)]
          (cond
            (and fsm-v-map? (not error-result?))
            (assoc
             {:state/value result
              :fsm/matched-state matched-state}
             :fsm/previous-state
             (or (:fsm/matched-state value) :fsm/initial))
            (and fsm-v-map? error-result?)
            (assoc
             {:state/error result
              :fsm/matched-state matched-state}
             :fsm/previous-state
             (or (:fsm/matched-state value) :fsm/initial))
            error-result? value
            (not (or error-result? fsm-v-map?)) result))))))

(defn complete
  "Completes the fsm by advancing through states until the same value is produced twice."
  {:malli/schema [:=> [:cat state-action-map :any [:* :any]] :any]}
  [fsm-map value & args]
  (let [fsm-states
        (iterate (fn [s] (apply advance fsm-map s args)) value)]
    (u/trace ::complete
      (apply concat (select-keys (meta fsm-map) [:fsm/name]))
      (reduce (fn [current-state next-state]
                (if (= current-state next-state)
                  (reduced current-state)
                  next-state))
              fsm-states))))
