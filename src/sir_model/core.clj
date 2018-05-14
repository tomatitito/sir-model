(ns sir-model.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [sir-model.util :as util]
            [sir-model.two-stage-poisson :as model]
            [com.climate.claypoole :as cp]
            [oz.core :as oz])
  (:use [anglican [core :exclude [-main]] runtime emit stat]))


(def arg-map
  {:t-max        30
   :compartments [:S :I :R :primary :secondary]
   :inits        {:S 100000 :I 2}
   :prior-1      (uniform-continuous 0.4 0.9)
   :prior-2      (uniform-continuous 0.8 1.2)
   :n-samples    10
   :n-thin        1
   :data         [5 20 100 120 200 100 300 700 1000 1400 1700 1600 1500 100 600 300 200 100 50 20 10 5 4]
   })


(def counter (agent 0))
(add-watch counter :watcher
           (fn [key agent old-state new-state]
             (println "sir-model.core: just started simulation" new-state)))


(defn force-sample
  "Evaluates a lazy-sample and increments counter."
  [query-results n]
  (send counter inc)
  (nth query-results n))


(defn lazy-samples
  "Takes an anglican query and returns samples as a lazy-seq."
  [anglican-query args]
  (doquery :smc anglican-query [args] :number-of-particles 1000))


(defn pmap-samples
  "Takes the result from an anglican doquery and an optional thinning parameter
  and extracts n elements out of that lazy-seq."
  [query-results n]
  (let [n-cpus (.. Runtime getRuntime availableProcessors)
        n-threads (+ n-cpus 2)]
    ;; if no logging to screen is needed, use the commented version
    ;(pmap #(nth query-result %) (range 0 n thin))
    (cp/upmap n-threads #(force-sample query-results %) (range 0 n))))


(defn sampler
  [anglican-query args n]
  (-> anglican-query
      (lazy-samples args)
      (pmap-samples n)))


(defn dashboard
  [samples]
  (let
    [primary (util/weekly-plot-spec samples :primary)
     secondary (util/weekly-plot-spec samples :secondary)
     new {:data     {:values (util/vec->vega-time-series (util/new-infections-in-seasons samples))}
          :mark     "tick"
          :encoding {:x {:field :week :type "ordinal"}
                     :y {:field :data :type "quantitative"}}}
     lambda-1 (util/histo-spec (util/from-results samples :lambda-1))
     lambda-2 (util/histo-spec (util/from-results samples :lambda-2))
     weekly-dists {:data     {:values (util/vec->vega-time-series (util/new-infections-in-seasons samples))}
                   :mark     "tick"
                   :encoding {:x {:field :data :type "quantitative"}
                              :y {:field :week :type "ordinal"}}}

     board {:hconcat
            [{:vconcat [primary secondary new {:hconcat [lambda-1 lambda-2]}]}
             weekly-dists]}
     ]

    (oz/v! board)))


(let [n-runs 5000
      samples (sampler model/two-stage-poisson-query arg-map n-runs)]
  (dashboard samples))


