(ns sir-model.cohort
  (:require [clojure.core.async
             :as as
             :refer [<! <!! >! >!! take! put! pipe go chan buffer close! thread alts! alts!! alt! timeout]]
            [sir-model.compartments :as compartments]
            )
  (:use [anglican [core :exclude [-main]] runtime emit]
        [anglican-code distributions prob_functions queries] ))




(defn progress
  "Takes a timestep, max number of timesteps and number of infected and begins progression."
  [t-cur t-max n-inf sync-chans recovery-param]
  (loop [t t-cur
         cases n-inf]

    (when (< t t-max)
      (println "Schleife:" t)

      (let
        [sync-chan (get sync-chans (keyword (str t)))
         still-inf (- cases 1)
         removed 1 ]

        ;(put! sync-chan (go (<! sync-chan (fn [x] (update-infected still-inf x)))))
        (go (>! sync-chan (compartments/update-IR still-inf removed (<! sync-chan))))
        (println "nach Schleife: " t)

        (recur (inc t) (- cases still-inf))))))


(defn progression
  "Takes a timestep, max number of timesteps and number of infected and begins progression."
  [t-cur t-max n-inf I-channel-for-t R-channel-for-t recovery-par]
  (go
    (loop [t t-cur
           cases n-inf]
      (when (< t t-max)

        (let [still-inf (I2R [cases recovery-par])
              rec (- cases still-inf)]
          (println "Schleife:" t)
          (>! I-channel-for-t rec)
          (>! R-channel-for-t still-inf)
          (recur (inc t) (- cases still-inf)))))))
