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
   :inits        {:S 1000000 :I 400}
   :prior-1      (uniform-continuous 0.4 0.9)
   :prior-2      (uniform-continuous 1.5 2.5)
   })

(defn prior [n thin]
  (->>
    (doquery :smc model/two-stage-poisson-query [arg-map model/form-and-prog] :number-of-particles 1000)
    (take n)
    (take-nth thin)))

(defn lazy-samples
  "Takes an anglican query and returns samples as a lazy-seq."
  [anglican-query]
  (doquery :smc anglican-query [arg-map model/form-and-prog] :number-of-particles 1000))

(defn from-query
  "Takes the result from an anglican doquery and an optional thinning parameter
  and extracts n elements out of that lazy-seq."
  ([query-results n]
   (from-query query-results n 1))
  ([query-result n thin]
   (pmap #(nth query-result %) (range 0 n thin))))

(defn from-query-agent
  "Same as from-query but using agents."
  ([query-result n]
   (from-query-agent query-result n 1))
  ([query-result n thin]
   (let
     [agt (agent (lazy-seq (first query-result)))
      n-runs (* thin n)]
     (doseq [i (range 0 n-runs thin)]
       (send agt conj (nth (rest query-result) i)))
     @agt)))

(def samples
  (->
    (lazy-samples model/two-stage-poisson-query)
    (from-query 5)))
;(def burned (time (from-query-agent samples 10 1)))


(defn -main
  "Probabilistic SIR-Model"
  [& args]

  ;(util.functions/write-seasons! burned util.functions/new-infections "data/season.csv")
  )














