(ns sir-model.compartments)


;(defrecord SIR-Compartments
;  [S I R] )
;
;
;(defn create-compartments-coll
;  "Creates a map of length timesteps with each entry being an SIR-Compartments record."
;  ([timesteps] (create-compartments-coll timesteps []))
;  ([timesteps coll] (into coll (repeatedly timesteps #(->SIR-Compartments 0 0 0)))))
;
;
;(defn initialize-compartments-coll
;  [coll n-susceptible initially-infected]
;  (-> coll
;      (assoc-in [0 :S] n-susceptible)
;      (assoc-in [0 :I] initially-infected)))
;
;
;(defn create-and-init-compartments-map
;  [timesteps n-susceptible initially-infected]
;  (-> timesteps
;      (create-compartments-coll)
;      (initialize-compartments-coll n-susceptible initially-infected)))


