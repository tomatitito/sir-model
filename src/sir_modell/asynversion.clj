(ns sir-modell.asynversion
  (:require [clojure.core.async
             :as as
             :refer [<! <!! >! >!! take! put! pipe go chan buffer close! thread alts! alts!! alt! timeout]]
            )
  (:use [anglican [core :exclude [-main]] runtime emit]
        [anglican-code distributions prob_functions queries]))

(defn create-I-channels
  "Creates a map of length timesteps with each entry being a map holding number the channel
  has been touched, required numner of touches, the number of infected individuals and an
  out-channel. When the channel has been touched required number of times, the number of
  infected is put on the channel."
  ([timesteps] (create-I-channels timesteps {}))
  ([timesteps channel-map]
   (if (pos? timesteps)
     (let [entry {:touched 0 :required timesteps :infected 0 :to-cohort (as/chan 1)}
           ts-key (keyword (str timesteps))
           ]
       (create-I-channels (dec timesteps) (assoc channel-map ts-key entry))
       )
     channel-map)))

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
