(ns sir-model.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [sir-model.util :as util]
            [sir-model.two-stage-poisson :as model]
            [com.climate.claypoole :as cp]
            [oz.core :as oz])
  (:use [anglican [core :exclude [-main]] runtime emit stat])
  (:import (java.util Arrays)))


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


;(let [n-runs 5000
;      samples (sampler model/two-stage-poisson-query arg-map n-runs)]
;  (dashboard samples))
;(def samples (sampler model/two-stage-poisson-query arg-map 10))

(defn samples->weekly-new
  "Takes samples and returns a seq of vectors holding weekly numbers of
  newly infected individuals"
  [samples]
  (->> samples
       (util/new-infections-in-seasons)
       (apply map vector)))




(defn hdi [samples cred-mass]
  "Compute highest density interval for probability distribution represented by samples. Algorithm is
  adopted from Kruschke, J.K. (2015)."
  (let [sorted (sort samples)
        ;;number of samples needed with given cred-mass
        n-keep (int (Math/floor (* cred-mass (count sorted))))
        ;;how many CIs to compare
        n-CIs (- (count sorted) n-keep)
        ;;computing widths for the different CIs
        ci-width (reduce #(conj %1 (- (nth sorted (+ %2 n-keep)) (nth sorted %2))) [] (range n-CIs))
        ;;keep the narrowest width
        min-width (apply min ci-width)
        ;;locate index of min-width
        ind-of-min (.indexOf ci-width min-width)
        ;;get hdi borders
        borders [(nth sorted ind-of-min) (nth sorted (+ ind-of-min n-keep))]
        ]
    borders))


(defn samples->hdi-borders
  "Compute highest density interval borders for samples as returned by anglican. Wrapper around hdi."
  [samples cred-mass]
  (let [weekly (samples->weekly-new samples arg-map)]
    (map #(hdi % cred-mass) weekly)))


(defn borders->vega-lite
  [borders]
  (let [lo (map first borders)
        hi (map second borders)]
    (into (util/vec->time-series lo) (util/vec->time-series hi))))

