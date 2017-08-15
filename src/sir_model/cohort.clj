(ns sir-model.cohort
  (:require [clojure.core.async
             :as as
             :refer [<! <!! >! >!! take! put! pipe go chan buffer close! thread alts! alts!! alt! timeout]]
            [sir-model.compartments :as compartments]
            )
  (:use [anglican [core :exclude [-main]] runtime emit]
        [anglican-code distributions prob_functions queries] ))



(defn I2R
  "As time progresses, with each unit of time a certain number of infected recover."
  [number-infected recovery-parameters]
  (sample* (binomial number-infected recovery-parameters)))


(defn commpute-still-infected [cases removed]
  "Make sure that number of still infected is not bigger than number of cases."
  (if (< cases removed)
    cases
    (- cases removed)))


(defn start-cohort
  [t-cur R-0 compartments-map]
  (let [already-inf (get-in compartments-map [(keyword (str t-cur)) :I])
        new-inf (get-in (first (take 1 (doquery :smc new-infections-model [R-0 already-inf]))) [:result :new-infections])]
    (update-in compartments-map [(keyword (str t-cur))] #(compartments/update-SI new-inf %))
    ))


(defn progress
  "Progression of a cohort. Returns the compartments-map when progression finishes."
  [t-cur t-max n-inf recovery-param compartments-map]
  (loop [t (inc t-cur)
         cases n-inf
         compartments compartments-map]

    (if (or (>= t t-max) (not (pos? cases)))
      compartments

      (let
        [susceptible (get-in compartments-map [(keyword (str t-cur)) :S])
         removed (I2R n-inf recovery-param)
         still-inf (commpute-still-infected cases removed)
         updated-compartments (update-in compartments [(keyword (str t))] #(compartments/update-IR susceptible still-inf removed %)) ]

        (recur
          (inc t)
          still-inf
          updated-compartments
          )))))
