(ns sir-model.compartments
  (:require [clojure.core.async :as as]))


(defrecord SIR-Compartments
  [S I R] )


(defn create-compartments-map
  "Creates a map of length timesteps with each entry being an SIR-Compartments record."
  ([timesteps] (create-compartments-map timesteps {}))
  ([timesteps channel-map]
   (if (pos? timesteps)
     (let [ts-key (keyword (str timesteps))
           sir-comp (SIR-Compartments. 0 0 0)
           ]
       (create-compartments-map (dec timesteps) (assoc channel-map ts-key sir-comp)))
     channel-map)) )


(defn initialize-compartments-map
  [compartments-map n-susceptible initally-infected]
  (-> compartments-map
      (assoc-in [:1 :S] n-susceptible)
      (assoc-in [:1 :I] initally-infected))
  )


(defn update-S [new-inf sir-record ]
  (assoc sir-record :S (- (:S sir-record) new-inf)))


(defn update-R [removed sir-record ]
  (assoc sir-record :R (+ (:R sir-record) removed)))


(defn update-I [still-infected sir-record]
  (assoc-in sir-record [:I ] (+ (:I sir-record) still-infected)))


(defn carry-over-S
  "Number of susceptibles needs to be retained in progression of cohort."
  [n-susceptible sir-record]
  (assoc sir-record :S n-susceptible))


(defn update-SI
  "Wraps update-S and update-I, because those functions are called at the start of a cohort."
  [new-inf sir-record]
  (->> sir-record
       (update-S new-inf)
       (update-I new-inf)))


(defn update-IR
  "During progression, this updates the I and R compartments and keeps track of number in S."
  [susceptible still-infected removed sir-record]
  (->> sir-record
       (carry-over-S susceptible)
       (update-R removed)
       (update-I still-infected) ))
