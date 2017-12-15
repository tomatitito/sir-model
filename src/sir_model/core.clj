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
   :n-samples    10
   :n-thin        1
   })


(defn lazy-samples
  "Takes an anglican query and returns samples as a lazy-seq."
  [anglican-query]
  (doquery :smc anglican-query [arg-map model/form-and-prog] :number-of-particles 1000))


(defn force-samples
  "Takes the result from an anglican doquery and an optional thinning parameter
  and extracts n elements out of that lazy-seq."
  ([query-results n]
   (force-samples query-results n 1))
  ([query-result n thin]
   (pmap #(nth query-result %) (range 0 n thin))))


(defmacro sampler
  "Samples from n-samples (optionally thinned) from anglican-query."
  [anglican-query n-samples & thin]
    `(let
       [macro-model# (:name (meta (var ~anglican-query)))
        n-thin# (if '(seq ~thin) ~@thin 1)
        n-runs# (* ~n-samples n-thin#)]
       (->
         (lazy-samples ~anglican-query)
         (force-samples n-runs# n-thin#)
         ;(with-meta {:model macro-model# :n-runs n-runs})
          )
       )
    )


(defmacro query-string
  [anglican-query]
  `(let
     [macro-model# (:name (meta (var ~anglican-query)))]
     (str macro-model#)))


(def samples (time (lazy-samples model/two-stage-poisson-query)))
(def forced (time (force-samples samples 3)))
(count forced)
;(first forced)

;(def samples (sampler model/two-stage-poisson-query 10 2))
;(count samples)
;(meta samples)


(defn -main
  "Probabilistic SIR-Model"
  [& args]

  ;(util.functions/write-seasons! forced util.functions/new-infections "data/testdat.csv")
  )














