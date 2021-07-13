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
            [site.fabricate.prototype.schema :as schema]
            [site.fabricate.sketch :as sketch]))


(def state-action-map
  "Malli schema for the map where each entry is a ⟨s,α⟩ tuple,
  \"where s is a state states and α is an action.\"
  The action performs the transition from s to a target state
  specified by the signature of α.

  In this implementation, states are defined by a malli schema.

  See Lamport [2008] - \"Computation and State Machines\""
  [:map-of [:fn schema/malli?] [:fn fn?]])


(defn advance
  "Takes a value, matches it against the schema keys defined in the
   input state-action map, and calls the appropriate function to advance
   the value to the next state."
  {:malli/schema [:=> [:cat state-action-map :any] :any]}
  [fsm-map value]
  (let [union-schema (schema/unify (keys fsm-map))
        parsed (m/parse union-schema value)]
    (if (= :malli.core/invalid parsed)
      (do
        (println "unmatched value"
                 (me/humanize (m/explain union-schema val)))
        value)
      (let [matched-schema (-> union-schema
                               (nth (inc (first parsed)))
                               last)
            op (get fsm-map matched-schema)]
        (do
          (println "advancing fsm:" (get (m/properties matched-schema)
                                         :description))
          (op value))))))

(defn complete [fsm-map value]
  ;; the most basic form of defining completeness: wait
  ;; for the FSM to return two successive identical values,
  ;; which works for both non-matching values and for "finished" values.
  ;;
  ;; this could be extended with custom equality semantics.
  ;;
  ;; In this implementation, any side effects specified in the final
  ;; action of the FSM will be performed twice.
  (let [fsm-states (iterate (partial advance fsm-map) value)]
    (reduce (fn [current-state next-state]
              (if (= current-state next-state)
                (reduced current-state)
                next-state))
            fsm-states)))
