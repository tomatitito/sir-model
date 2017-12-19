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
  [anglican-query args]
  (doquery :smc anglican-query [args model/form-and-prog] :number-of-particles 1000))


(defn force-samples
  "Takes the result from an anglican doquery and an optional thinning parameter
  and extracts n elements out of that lazy-seq."
  ([query-results n]
   (force-samples query-results n 1))
  ([query-result n thin]
   (pmap #(nth query-result %) (range 0 n thin))))


(defn sampler
  [anglican-query args n thin]
  (-> anglican-query
      (lazy-samples args)
      (force-samples n thin)))


(defmacro query-string
  [anglican-query]
  `(let
     [macro-model# (:name (meta (var ~anglican-query)))]
     (str macro-model#)))

;; TODO: compute path

(defn -main
  "Probabilistic SIR-Model"
  [population initially-infected n-runs & thin]

  (let
    [args (assoc arg-map :inits {:S (Integer/parseInt population) :I (Integer/parseInt initially-infected)})
     thin-par (if thin (Integer/parseInt (first thin)) 1)
     samples (sampler model/two-stage-poisson-query args (Integer/parseInt n-runs) thin-par)
     getter-fns [util.functions/new-infections
           #(util.functions/from-season % :S)
           #(util.functions/from-season % :I)
           #(util.functions/from-season % :R)
           #(util.functions/from-season % :primary)
           #(util.functions/from-season % :secondary)]
     path "data/uberjartest.csv"
     header ["week" "new" "S" "I" "R" "primary" "secondary" "sim_id"]]

    (util.functions/write-seasons! samples getter-fns path header)))

;(time (-main 100 1 50 4))
