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
   :inits        {:S 10000 :I 200}
   :prior-1      (uniform-continuous 0.6 0.8)
   :prior-2      (uniform-continuous 1.5 2.5)
   })

(def samples (doquery :smc model/two-stage-poisson-query [arg-map model/form-and-prog] :number-of-particles 1000))
(def burned (take-nth 4 (take 500 samples)))

(defn -main
  "Probabilistic SIR-Model"
  [& args]

  (util.functions/write-seasons! burned util.functions/new-infections "data/testdat.csv") )
(-main)




