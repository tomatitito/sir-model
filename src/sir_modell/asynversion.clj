(ns sir-modell.asynversion
  (:require [clojure.core.async
             :as as
             :refer [<! <!! >! >!! take! put! pipe go chan buffer close! thread alts! alts!! alt! timeout]]
            )
  (:use [anglican [core :exclude [-main]] runtime emit]
        [anglican-code distributions prob_functions queries]))


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


(defrecord I-compartment
  [infected countdown outchannel])


(defrecord SIR-Compartments
  [S I R] )


(defn sync-channels
  "Use this to coordinate access to the compartments. Creates a map of length
   timesteps with each entry being a map holding a timestep-channel-pair. An
    SIR-Compartments record is put on every channel."
  ([timesteps] (sync-channels timesteps {}))
  ([timesteps channel-map]
   (if (pos? timesteps)
     (let [ts-key (keyword (str timesteps))
           comp-c (chan 1)
           ;sir-map (create-sir-comp timesteps)
           i-comp (I-compartment. 0 timesteps (as/chan))
           sir-comp (SIR-Compartments. 0 i-comp 0)
           ]
       (as/put! comp-c sir-comp)
       (sync-channels (dec timesteps) (assoc channel-map ts-key comp-c)))
     channel-map)))

(defn update-S [new-inf sir-record ]
  (assoc sir-record :S (- (:S sir-record) new-inf)))

(defn update-R [removed sir-record ]
  (assoc sir-record :R (+ (:R sir-record) removed)))

(defn countdown [sir-record]
  (update-in sir-record [:I :countdown] dec))

(defn update-infected [still-infected sir-record]
  (assoc-in sir-record [:I :infected] (+ (:infected (:I sir-record)) still-infected)))

(defn notify-cohort [sir-record]
  (if (= 0 (get-in sir-record [:I :countdown]))
    (let [outchannel (get-in sir-record [:I :outchannel])
          n-infected (get-in sir-record [:I :infected])]
      (put! outchannel n-infected))
    )
  sir-record)

(defn update-I
  [still-infected sir-record]
  (->> sir-record
       (countdown)
       (update-infected still-infected)
       (notify-cohort)))

(defn update-IR
  "Apply all neccesary functions to update the I and R-compartments during the progression
  of a cohort. The number of individuals that are still infected is added to the I-Compartemt
  of the current timestep. Likewise, the number of people that are removed is added to the R-
  Compartment."
  [still-infected removed sir-record]
  (->> sir-record
       ;(println)
       (update-R removed)
       (update-I still-infected)
       ))


(defn progression2
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

        (put! sync-chan (go (<! sync-chan (fn [x] (update-infected still-inf x)))))
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
