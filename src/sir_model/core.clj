(ns sir-model.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [util.functions :as util]
            [sir-model.two-stage-poisson :as model]
            [com.climate.claypoole :as cp]
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


(def counter (agent 0))
(add-watch counter :watcher
           (fn [key agent old-state new-state]
             (println "watcher: just started" new-state)))


(defn force-sample
  [query-results n]
  (send counter inc)
  (nth query-results n))


(defn lazy-samples
  "Takes an anglican query and returns samples as a lazy-seq."
  [anglican-query args]
  (doquery :smc anglican-query [args model/form-and-prog] :number-of-particles 1000))


(defn pmap-samples
  "Takes the result from an anglican doquery and an optional thinning parameter
  and extracts n elements out of that lazy-seq."
  ([query-results n]
   (pmap-samples query-results n 1))
  ([query-results n thin]
   ;; if no logging to screen is needed, use the commented version
   ;(pmap #(nth query-result %) (range 0 n thin))
   (cp/upmap 6 #(force-sample query-results %) (range 0 n thin))))


(defn sampler
  [anglican-query args n thin]
  (-> anglican-query
      (lazy-samples args)
      (pmap-samples n thin)))


(defmacro query-string
  [anglican-query]
  `(let
     [macro-model# (:name (meta (var ~anglican-query)))]
     (str macro-model#)))


(defn compute-filename [args]
  [(.toString (java.time.LocalDateTime/now))
   "two-stage-poisson-query"
   (get-in args [:inits :S])
   (get-in args [:inits :I])])


(defn -main
  "Probabilistic SIR-Model"
  [population initially-infected n-runs & thin]

  (let
    [args (assoc arg-map :inits {:S population :I initially-infected})
     thin-par (if thin (first thin) 1)
     samples (sampler model/two-stage-poisson-query args n-runs thin-par)
     getter-fns [util.functions/new-infections
           #(util.functions/from-season % :S)
           #(util.functions/from-season % :I)
           #(util.functions/from-season % :R)
           #(util.functions/from-season % :primary)
           #(util.functions/from-season % :secondary)]
     filename (clojure.string/join "_" (conj (compute-filename args) n-runs))
     filedir "data"
     path (str filedir "/" filename ".csv")
     header ["week" "new" "S" "I" "R" "primary" "secondary" "sim_id"]]

    (util.functions/write-seasons! samples getter-fns path header)))

;(time (-main 1000 1 50  ))

