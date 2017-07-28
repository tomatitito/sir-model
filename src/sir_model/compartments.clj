(ns sir-model.compartments
  (:require [clojure.core.async :as as]))


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
           comp-c (as/chan 1)
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
      (as/put! outchannel n-infected))
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
       (update-R removed)
       (update-I still-infected) ))
