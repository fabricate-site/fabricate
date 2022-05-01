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
            [site.fabricate.prototype.schema :as schema]))

(def state-action-map
  "Malli schema for the map where each entry is a ⟨s,α⟩ tuple,
  \"where s is a state states and α is an action.\"
  The action performs the transition from s to a target state
  specified by the signature of α.

  In this implementation, states are defined by a malli schema.

  See Lamport [2008] - \"Computation and State Machines\""
  (m/schema [:map-of [:fn schema/malli?] [:fn ifn?]]))

(defn advance
  "Takes a value, matches it against the schema keys defined in the
   input state-action map, and calls the appropriate function to advance
   the value to the next state.

  Additional arguments to the function can be supplied via & args; they will be
  appended to the arguments passed to the matched function via apply.

  Errors in function execution will be printed and the input value will be returned."
  {:malli/schema [:=> [:cat state-action-map :any [:* :any]] :any]}
  [fsm-map value & args]
  (let [union-schema (schema/unify (keys fsm-map))
        parsed (m/parse union-schema value)]
    (if (= :malli.core/invalid parsed)
      (do
        (println "unmatched value"
                 (me/humanize (m/explain union-schema val)))
        value)
      (let [matched-schema (-> union-schema
                               m/children
                               (nth (first parsed) #_(inc (first parsed)))
                               last)
            op (get fsm-map matched-schema
                    (get fsm-map (m/form matched-schema)
                         (fn [v] (println "unmatched value") v)))]
        (do
          (println "advancing fsm:" (get (m/properties matched-schema)
                                         :fsm/description))
          (try (apply op value args)
               (catch Exception e
                 (println (Throwable->map e))
                 value)))))))

(defn complete
  "Completes the fsm by advancing through states until the same value is produced twice."
  {:malli/schema [:=> [:cat state-action-map :any [:* :any]] :any]}
  [fsm-map value & args]
  (let [fsm-states (iterate #(apply advance fsm-map % args) value)]
    (reduce (fn [current-state next-state]
              (if (= current-state next-state)
                (reduced current-state)
                next-state))
            fsm-states)))
