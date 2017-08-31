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


(defn create-and-init-compartments-map
  [timesteps n-susceptible initally-infected]
  (-> (create-compartments-map timesteps)
      (assoc-in [:1 :S] n-susceptible)
      (assoc-in [:1 :I] initally-infected)))


(defn carry-over-S
  "Number of susceptibles needs to be retained in progression of cohort."
  [n-susceptible sir-record]
  (assoc sir-record :S n-susceptible))


