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
   :inits        {:S 100000 :I 40}
   :prior-1      (uniform-continuous 0.4 0.9)
   :prior-2      (uniform-continuous 1.5 2.5)
   })

(defn prior [n thin]
  (->>
    (doquery :smc model/two-stage-poisson-query [arg-map model/form-and-prog] :number-of-particles 1000)
    (take n)
    (take-nth thin)))

(def samples (doquery :smc model/two-stage-poisson-query [arg-map model/form-and-prog] :number-of-particles 1000))
;(def burned (take-nth 4 (take 1000 samples)))

(defn from-query-agent
  "Same as from query but using agents."
  ([query-result n]
   (from-query-agent query-result n 1))
  ([query-result n thin]
   (let
     [agt (agent (lazy-seq (first query-result)))
      n-runs (* thin n)]
     (doseq [i (range 0 n-runs thin)]
       (send agt conj (nth (rest query-result) i)))
     @agt)))

(defn from-query
  "Takes the result from an anglican doquery and an optional thinning parameter
  and extracts n elements out of that lazy-seq."
  ([query-results n]
    (from-query query-results n 1))
  ([query-result n thin]
   (pmap #(nth query-result %) (range 0 n thin))))

(def burned (from-query samples 5000 4))

(defn -main
  "Probabilistic SIR-Model"
  [& args]

  (util.functions/write-seasons! burned util.functions/new-infections "data/testdat.csv"))











