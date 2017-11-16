(ns sir-model.cohort
  (:require [sir-model.compartments :refer :all] )
  (:use [anglican [core :exclude [-main]] runtime emit]
        [anglican-code distributions prob_functions queries] ))


(defm start-cohort
      [t-cur lambda-old lambda-new compartments-coll & data]
      (let [old-n (get-in compartments-coll [t-cur :I])
            new-n (sample (poisson (* old-n lambda-old)))
            secondary (sample (poisson (* new-n lambda-new)))
            weekly-cases (+ new-n secondary)
            still-susceptible (- (get-in (first compartments-coll) [:S]) weekly-cases)]

        ;; if data available
        (when data
          (observe (poisson (+ (* old-n lambda-old) (* new-n lambda-new))) (first data)))

        ;; update compartments-map based on computations
        (-> compartments-coll
            (assoc-in ,,, [t-cur :I] (+ (get-in compartments-coll [t-cur :I]) weekly-cases))
            (assoc-in ,,, [t-cur :S] (- (get-in compartments-coll [t-cur :S]) weekly-cases)))))


(defm progress
  "Progression of a cohort. After new individuals have been infected (which happened in the function start-cohort), at
   each timestep (starting one timestep after the start of the cohort) some people from the cohort recover. Progression
   continues until time is up. Returns the compartments-map."
  [t-cur recovery-param compartments-map]
  (loop [;; recovery starts at the timestep after infection
         ;; this makes sure that all newly infected can generate
         ;; new cases at their time of infection
         t (inc t-cur)
         cases (get-in compartments-map [t-cur :I])
         compartments compartments-map]

    (if (= t (count compartments-map))
      compartments

      (let
        [;; IMPORTANT: number of susceptibles has to remain
         ;; constant during progression
         susceptible (get-in compartments-map [t-cur :S])

         removed (sample* (binomial cases recovery-param))
         still-inf (max 0 (- cases removed))

         updated-compartments
         (-> compartments
             (assoc-in ,,, [t :S] susceptible)
             (assoc-in ,,, [t :I] (+ (get-in compartments [t :I]) still-inf))
             (assoc-in ,,, [t :R] (+ (get-in compartments [t :R]) (get-in compartments [(dec t) :R]) removed)))]

        (recur (inc t)
               still-inf
               updated-compartments)))))


(defm cohort-progression
      [t-cur lambda-old lambda-new recovery-param compartments-coll & data]
      (if data
        (progress t-cur recovery-param
                  (start-cohort t-cur lambda-old lambda-new compartments-coll (first data)))
        (progress t-cur recovery-param
                  (start-cohort t-cur lambda-old lambda-new compartments-coll))))


;(defm season
;      [progression-fun n-weeks configuration-data]
;      (let [compartments-coll (create-and-init-compartments-map)]))
