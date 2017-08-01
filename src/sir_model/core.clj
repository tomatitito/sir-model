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
  ;(assoc s :S (+ 42 (:S s)))


  (def sc (create-compartments-map 4))

  ;(a/go
  ;  (a/>! (get sc :2) (update-IR 43 47 (a/<! (get sc :2))))
  ;
  ;  (a/take! (get sc :2) println)
  ;  )


  (progress 1 5 100 sc 0.4)
  )

