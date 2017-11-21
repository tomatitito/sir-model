(ns sir-model.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [util.functions :as util]
            )
  (:use [anglican [core :exclude [-main]] runtime emit stat]
        sir-model.cohort))

;(take-nth 5 (drop 500 (take 1000 samples))

(defn -main
  "Probabilistic SIR-Model"
  [& args]

  (def samples (doquery :smc simple-poisson-process-model [10 cohort-progression 70000000 30] :particles 1000))
  (def burned (take-nth 10 (drop 100 (take 500 samples))))
  (util.functions/write-seasons! burned :unused "data/testdat.csv")

  )


