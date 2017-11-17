(ns sir-model.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [util.functions :as util])
  (:use [anglican [core :exclude [-main]] runtime emit stat]
        [anglican-code prob_functions distributions queries]
        util.functions
        sir-model.cohort
        sir-model.cohort-test
        ;sir-model.compartments
    ;proto-repl-charts.charts
        ))


;; create and initialize compartments-coll
;; TODO: rename function to create-and-init-compartments-coll
(def initial-coll
  (create-and-init-compartments-map 10 100000 500))


(defn -main
  "Probabilistic SIR-Model"
  [& args]

  (def samples (doquery :lmh simple-poisson-process-model [10 cohort-progression 70000000 30]))

  )


