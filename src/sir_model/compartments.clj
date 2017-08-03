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
     channel-map)))


(defn update-S [new-inf sir-record ]
  (assoc sir-record :S (- (:S sir-record) new-inf)))


(defn update-R [removed sir-record ]
  (assoc sir-record :R (+ (:R sir-record) removed)))


(defn update-I [still-infected sir-record]
  (assoc-in sir-record [:I ] (+ (:I sir-record) still-infected)))


(defn update-SI
  "Wraps update-S and update-I, because those functions are called at the start of a cohort."
  [new-inf sir-record]
  (->> sir-record
       (update-S new-inf)
       (update-I new-inf)))


(defn update-IR
  "Wrapps update-I and update-R, because those functions are called together during progression of a cohort."
  [still-infected removed sir-record]
  (->> sir-record
       (update-R removed)
       (update-I still-infected) ))
