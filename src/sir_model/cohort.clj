(ns sir-model.cohort
  (:require [clojure.core.async
             :as as
             :refer [<! <!! >! >!! take! put! pipe go chan buffer close! thread alts! alts!! alt! timeout]]
            [sir-model.compartments :as compartments]
            )
  (:use [anglican [core :exclude [-main]] runtime emit]
        [anglican-code distributions prob_functions queries] ))


(defm start-cohort
      [t-cur compartments-map  lambda-old lambda-new]
      (let [
            old-n (get-in compartments-map [(keyword (str t-cur)) :I])
            new-n (sample (poisson (* old-n lambda-old)))
            secondary (sample (poisson (* new-n lambda-new)))
            weekly-cases (+ new-n secondary)]

        ;; if data available
        ;(observe (poisson (+ (* old-n lambda-old) (* new-n lambda-new))) datapoint)

        ;; update compartments-map based on computations
        (-> compartments-map
            (assoc-in ,,, [(keyword (str t-cur)) :I] (+ (get-in compartments-map [(keyword (str t-cur)) :I]) weekly-cases))
            (assoc-in ,,, [(keyword (str t-cur)) :S] (- (get-in compartments-map [(keyword (str t-cur)) :S]) weekly-cases)) )
        ))


(defm progress
  "Progression of a cohort. After new individuals have been infected (which happened in the function start-cohort), at
   each timestep (starting one timestep after the start of the cohort) some people from the cohort recover. Progression
   continues until time is up. Returns the compartments-map."
  [t-cur t-max recovery-param compartments-map]
  (loop [t (inc t-cur)                                      ;recovery starts at the timestep after infection
         cases (get-in compartments-map [(keyword (str t-cur)) :I])
         compartments compartments-map]

    (if (> t (count compartments-map))
      compartments

      (let
        [current-t-key (keyword (str t))
         last-t-key (keyword (str (dec t)))

         susceptible (get-in compartments-map [(keyword (str t-cur)) :S]) ;for bookkeeping, number of susceptibles has to be constant during progression
         removed (sample* (binomial cases recovery-param))
         still-inf (max 0 (- cases removed))

         updated-compartments
         (-> compartments
             (assoc-in ,,, [current-t-key :S] susceptible)
             (assoc-in ,,, [current-t-key :I] (+ (get-in compartments [current-t-key :I]) still-inf))
             (assoc-in ,,, [current-t-key :R] (+ (get-in compartments [current-t-key :R]) (get-in compartments [last-t-key :R]) removed)))
         ]

        (recur
          (inc t)
          still-inf
          updated-compartments
          )))))
