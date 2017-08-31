(ns sir-model.cohort
  (:require [clojure.core.async
             :as as
             :refer [<! <!! >! >!! take! put! pipe go chan buffer close! thread alts! alts!! alt! timeout]]
            [sir-model.compartments :as compartments]
            )
  (:use [anglican [core :exclude [-main]] runtime emit]
        [anglican-code distributions prob_functions queries] ))


(defn start-cohort
  [t-cur R-0 compartments-map]
  (let [already-inf (get-in compartments-map [(keyword (str t-cur)) :I])
        new-inf (get-in (first (take 1 (doquery :smc new-infections-model [R-0 already-inf]))) [:result :new-infections])]

    ;(println "initial compartments-map:" compartments-map)
    (println "Already infected at beginning of process" already-inf ", new infections in cohort:" new-inf)

    ;flow of people from S to I
    ;(update-in compartments-map [(keyword (str t-cur))] #(compartments/update-SI new-inf %))
    (assoc-in compartments-map [(keyword (str t-cur)) :S] (- (get-in compartments-map [(keyword (str t-cur)) :S]) new-inf))
    (assoc-in compartments-map [(keyword (str t-cur)) :I] (+ (get-in compartments-map [(keyword (str t-cur)) :I]) new-inf))

    ;(println "updated compartments-map:" compartments-map)

    compartments-map
    ))


(defn progress
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
