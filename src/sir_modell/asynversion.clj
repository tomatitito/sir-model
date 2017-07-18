(ns sir-modell.asynversion
  (:require [clojure.core.async
             :as as
             :refer [<! <!! >! >!! take! put! pipe go chan buffer close! thread alts! alts!! alt! timeout]]
            )
  (:use [anglican [core :exclude [-main]] runtime emit]
        [anglican-code distributions prob_functions queries]))

(defn get-still-infected [n]
  (- n 2))

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
