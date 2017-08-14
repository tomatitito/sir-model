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

  ;create and initialize compartments-map
  (def sc (create-and-init-compartments-map 4 90 20))

  (println sc)

  ;progression of a cohort
  (progress 1 5 100  (start-cohort 1 sc 3) 0.4)
  )

