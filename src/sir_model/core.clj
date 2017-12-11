(ns sir-model.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [util.functions :as util]
            [sir-model.two-stage-poisson :as model]
            )
  (:use [anglican [core :exclude [-main]] runtime emit stat]))




(def arg-map
  {:t-max        40
   :compartments [:S :I :R :primary :secondary]
   :inits        {:S 60000000 :I 1000}
   :prior-1      (uniform-continuous 0.4 0.9)
   :prior-2      (uniform-continuous 1.4 2.8)
   })

(def samples (doquery :smc model/two-stage-poisson-query [arg-map model/form-and-prog] :number-of-particles 1000))
(def burned (take-nth 4 (take 1000 samples)))

(defn -main
  "Probabilistic SIR-Model"
  [& args]

  (util.functions/write-seasons! burned util.functions/new-infections "data/season.csv") )




