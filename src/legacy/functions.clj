(ns sir-modell.functions
  (:use [anglican core runtime emit]
        anglican-code.distributions
        anglican-code.queries
        util.functions)
  (:require [clojure.core.async :as as]))


(defn compute-new-infections [I-map I-counter-map S-map R-map recovery-rate R-0 current-timestep max-timesteps]
  "Compute new infections for every timestep as a probabilistic function of
  current infections. Update number of infected and susceptible individuals
  accordingly."
  (let [current-infections (get I-map (keyword (str current-timestep)))]
    ;TODO: Update S-map
    (take 1 (doquery :smc new-infections-model [R-0 (deref current-infections) ]))))




(defn watch-test-fn [key watched old-state new-state]
  (let [touched (:actual new-state)
        needed (:required new-state)]
    ;(println new-state)
    (println (str key " old state: " old-state " new state: " new-state))))


(defn create-I-map
  "Creates a map holding
  - the number of times the datastructure has been touched,
  - the required numner of touches,
  - the number of infected individuals and
  - an out-channel.
  When touched = required, the number of infected can be put on the out-channel."
  [timestep]
  {:touched 0 :required timestep :infected 0 :to-cohort (as/chan 1)} )


(defn create-sir-comp
  "Creates a map holding SIR-compartments."
  [t]
  {:S 0 :R 0 :I (create-I-map t) })

