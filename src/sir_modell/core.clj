(ns sir-modell.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [util.functions :as util])
  (:use [anglican [core :exclude [-main]] runtime emit stat]
        [anglican-code prob_functions distributions queries]
        sir-modell.functions
        sir-modell.datastructures
        util.functions
    ;proto-repl-charts.charts
        ))

;Setting up data structures. Maybe do this functionally?


(defn -main
  "Probabilistisches SIR-Modell"
  [& args]

  (def timesteps 20)
  (def cur-infs 300)
  (def rec-rate 0.4)
  (def I (ref-type-map ref timesteps))
  (def R (ref-type-map ref timesteps))


  (def I-counter (create-I-counter {} timesteps watch-test-fn))

  (cohort cur-infs rec-rate I I-counter R 0 timesteps)
  (print-infections-in-cohort I)
  ;(print-infections-in-cohort I-counter)
  (print-infections-in-cohort R)

  )

