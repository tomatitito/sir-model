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
        sir-model.cohort-test
        sir-model.compartments
    ;proto-repl-charts.charts
        ))


(defn -main
  "Probabilistic SIR-Model"
  [& args]

  ;create an instance of SIR-Compartments
  (def s (->SIR-Compartments 100 21 42))

  ;create and initialize compartments-map
  (def cm (create-and-init-compartments-map 4 100 4))

  (println cm)

  ;start and progression of a cohort
  (def testrun (->> cm
                 (start-cohort 1 3)
                 (progress 1 4 0.4)))

  )


