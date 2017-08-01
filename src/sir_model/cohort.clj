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

(defn progress
  "Progression of a cohort. Returns the compartments-map when progression finishes."
  [t-cur t-max n-inf compartments-map recovery-param]
  (loop [t t-cur
         cases n-inf
         compartments compartments-map]

    (if (and (< t t-max) (pos? cases))
      (do
        ;for debugging
        (println "Schleife:" t)

        (let
          [removed (I2R n-inf recovery-param)
           still-inf (- cases removed)
           updated-compartments (update-in compartments-map [(keyword (str t))] #(compartments/update-IR still-inf removed %))]

          ;for debugging
          (println "nach Schleife: " t)

          (recur
            (inc t)
            still-inf
            updated-compartments)))
      compartments)))
