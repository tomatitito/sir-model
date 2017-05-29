(ns sir-modell.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv])
  (:use [anglican
         [core :exclude [-main]]
         runtime
         emit
         stat]
        anglican-code.prob_functions
        clojure-code.functions
        proto-repl-charts.charts
        clojure-code.functions))

(defn -main
  "Probabilistisches SIR-Modell"
  [& args]
  ;;--------------------------------------------NEUER VERSUCH-----------------------------------
  ;(defquery sir-modell []
  ;  (let [
  ;        mean-duration (+ 1 (sample (poisson 4 )))
  ;        I (sample(beta 3000 6000))
  ;        R (sample (beta 6000 3000))
  ;        Infections (incidence-over-time [I R mean-duration [R] 10])
  ;        ]
  ;    ;(println Infections)
  ;    (println (change-in-R [I (+  1(sample (poisson 5)))]))
  ;    (println ["new-R: " (sample(poisson (+ R (change-in-R [I mean-duration]))))])
  ;    (println (sample (poisson 3.46)))
  ;    ;{:R R  :dur mean-duration}
  ;    )
  ;  )

  ;(def res (take 50 (doquery :lmh sir-modell nil)))
  ;
  ;;(write-to-file res :R "data/results.dat")
  ;---------------------------------------------------------------------------------------------
  (with-primitive-procedures [geometric]
   (defquery  sir []
    (let
    ; Starting point for a node at some point in time
     [N 700000
      I0 300
      transmission-prob 0.3
      susceptible (N2S [700000 0.3])
      exposed (encounters [3 0.5])
      infected (S2I [exposed transmission-prob])
      drop-outs (multinomial [infected [0.3 0.4 0.2 0.1]])
      testparam (sample (beta 1 1))]
     (observe* (geometric testparam) 3)
     ;(map #(observe (geometric testparam %) [2 3 2 2 5 3 5 6]))
     ; This was not a process yet but only an estimation of the numbers that go into the
     ; process. The Process itself starts now.
     ;course (I2R [1 2 3])]

     {:S susceptible, :E exposed :I infected :drops drop-outs :testparam testparam})))



  (def res (take 1 (doquery :lmh sir nil)))
  (println res)

  (proto-repl-charts.charts/line-chart
   "Trigonometry"
   {"sin" (map #(Math/sin %) (range 0.0 6.0 0.2))
    "cos" (map #(Math/cos %) (range 0.0 6.0 0.2))})


  (def e (extract-result res :E))
  ;(proto-repl-charts.charts/bar-chart "chart" {"E"  [3 4 5 3]})
  (frequencies e)
  (proto-repl-charts.charts/bar-chart "chart" (from-frequencies (frequencies e) 1)) :labels (from-frequencies (frequencies e) 0))
