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



(defquery
  simple-poisson-process-model
  [compartments-map]
  (let
    [
     shape (sample (gamma 20 5))
     R-0-dist (gamma shape 5)
     R-0 (sample R-0-dist)
     ;comp-map (progress 1 4 0.4
     ;                   (start-cohort 1 R-0 compartments-map))
     lambda-old (sample (uniform-continuous 0.5 1.5))
     lambda-new (sample (uniform-continuous 1.5 2.5))

     comp-map (progress 1 10 0.5 (start-cohort 1 compartments-map lambda-old lambda-new))]

    (observe (poisson (* 13 R-0)) 14)

    {:comp-map comp-map :l-old lambda-old :l-new lambda-new}))


(defn -main
  "Probabilistic SIR-Model"
  [& args]

  ;create an instance of SIR-Compartments
  (def s (->SIR-Compartments 100 21 42))

  ;create and initialize compartments-map
  (def cm (create-and-init-compartments-map 10 1000 10))

  (println cm)


  (def samples (doquery :lmh simple-poisson-process-model [ cm]))

  ;; extracting :I from result
  (def one (reduce #(conj %1 (get-in %2 [:result :comp-map :1 :I])) [] (take 20 samples)))
  (def two (reduce #(conj %1 (get-in %2 [:result :comp-map :2 :I])) [] (take 20 samples)))
  (def three (reduce #(conj %1 (get-in %2 [:result :comp-map :3 :I])) [] (take 20 samples)))
  (def four (reduce #(conj %1 (get-in %2 [:result :comp-map :4 :I])) [] (take 20 samples)))
  (def r-0 (reduce #(conj %1 (get-in %2 [:result :R-0])) [] (take 20 samples)))


  ;;start and progression of a cohort
  ;(def testrun (->> cm
  ;               (start-cohort 1 3)
  ;               (progress 1 4 0.4)))

  )


