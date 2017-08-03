(ns sir-model.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [util.functions :as util]
            [clojure.core.async :as a]
            )
  (:use [anglican [core :exclude [-main]] runtime emit stat]
        [anglican-code prob_functions distributions queries]
        util.functions
        sir-model.cohort
        sir-model.compartments
    ;proto-repl-charts.charts
        ))


(defn -main
  "Probabilistic SIR-Model"
  [& args]

  ;create an instance of SIR-Compartments
  (def s (->SIR-Compartments 100 21 42))

  ;create a compartments-map
<<<<<<< HEAD
  (def sc (create-and-init-compartments-map 4 90 20))
=======
  ;(def sc (initialize-compartments-map (create-compartments-map 4) 1000 100))
  (def sc (create-and-init-compartments-map 4 90 20))
  ;initially infected and susceptible
  ;(def sc (assoc-in sc [:1 :I] 100))
  ;(def sc (assoc-in sc [:1 :S] 1000))
>>>>>>> b65e1fe70c72d9dc4a2c34604e42d199cd4602f7

  (println sc)

  ;progression of a cohort
  (progress 1 5 100  (start-cohort 1 sc 3) 0.4)
  )

