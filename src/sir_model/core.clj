(ns sir-modell.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [util.functions :as util]
            [clojure.core.async :as a]
            )
  (:use [anglican [core :exclude [-main]] runtime emit stat]
        [anglican-code prob_functions distributions queries]
        sir-modell.functions
        sir-modell.datastructures
        util.functions
        sir-modell.cohort
    ;proto-repl-charts.charts
        ))

;Setting up data structures. Maybe do this functionally?


(defn -main
  "Probabilistisches SIR-Modell"
  [& args]

  ;create an instance of SIR-Compartments
  (def s (->SIR-Compartments 100 (->I-compartment 50 0 (a/chan)) 0))
  ;(assoc s :S (+ 42 (:S s)))


  (def sc (sync-channels 4))

  (a/go
    (a/>! (get sc :2) (update-IR 43 47 (a/<! (get sc :2))))

    (a/take! (get sc :2) println)
    )


  ;(defn mod-and-print [some-par sir-rec]
  ;  (println (update-R 42 (update-infected some-par (countdown sir-rec)))))

  ;(a/put! (get sc :2)
  ;        (a/go
  ;          (a/take! (get sc :2) (fn [x] (mod-and-print 43 x)))))


  ;(progression2 1 5 100 sc 0.4)
  ;(a/take! (get sc :1) println)
  ;(a/go (a/<! (get sc :1)))
  ;(progression2 2 3 (a/take! (get sc :2) println) sc 0.4)
  ;(progression2 3 3 (a/take! (get sc :3) println) sc 0.4)

  ;(a/go
  ;  (a/>! (get Cs :1) 43)
  ;  (a/take! (get Cs :1) println))
  )

