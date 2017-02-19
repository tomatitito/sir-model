(ns sir-modell.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv])
  (:use [anglican 
         [core :exclude [-main]] 
         runtime 
         emit 
         stat ]
        anglican-code.prob_functions
        clojure-code.functions))

(defn -main
  "Probabilistisches SIR-Modell"
  [& args]
  
  (defquery sir-modell []
    (let [
          mean-duration (+ 1 (sample (poisson 4 )))
          I (sample(beta 3000 6000))
          R (sample (beta 6000 3000))
          Infections (incidence-over-time [I R mean-duration [R] 10])
          ]
      ;(println Infections)
      (println (change-in-R [I (+  1(sample (poisson 5)))]))
      (println ["new-R: " (sample(poisson (+ R (change-in-R [I mean-duration]))))])
      (println (sample (poisson 3.46)))
      ;{:R R  :dur mean-duration}
      )
    )

  (def res (take 50 (doquery :lmh sir-modell nil)))
  
  ;(write-to-file res :R "data/results.dat")

  )
