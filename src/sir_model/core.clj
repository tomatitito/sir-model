(ns sir-model.core
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [sir-model.util :as util]
            [sir-model.two-stage-poisson :as model]
            [com.climate.claypoole :as cp]
            [oz.core :as oz]
            [clojure.tools.cli :refer [parse-opts]])
  (:use [anglican [core :exclude [-main]] runtime emit stat])
  )


(def arg-map
  {:t-max        40
   :compartments [:S :I :R :primary :secondary]
   :inits        {:S 100000 :I 2}
   :prior-1      (uniform-continuous 0.4 0.9)
   :prior-2      (uniform-continuous 1.0 1.8)
   :n-samples    10
   :n-thin        1
   ;:data         [5 20 100 120 200 100 300 700 1000 1400 1700 1600 1500 1000 600 300 200 100 50 20 10 5 4]
   })

(def data [5 20 100 120 200 100 300 700 1000 1400 1700 1600 1500 1000 600 300 200 100 50 20 10 5 4])

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
  ([anglican-query args n-particles]
   (doquery :ipmcmc anglican-query [args] :number-of-particles n-particles))
  ([anglican-query args]
    (lazy-samples anglican-query args 1000)))


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
  ([anglican-query args n n-particles]
   (-> anglican-query
       (lazy-samples args n-particles)
       (pmap-samples n)))
  ([anglican-query args n]
   (-> anglican-query
       (lazy-samples args)
       (pmap-samples n))))

(defn write-lambda-priors!
  [args n outfile]
  (let [l-1-prior (repeatedly n #(sample* (:prior-1 args)))
        l-2-prior (repeatedly n #(sample* (:prior-2 args)))
        dat-out (->> (interleave l-1-prior l-2-prior)
                     (partition 2))]
    (with-open [writer (io/writer outfile)]
      (csv/write-csv writer [["l_1_prior" "l_2_prior"]])
      (csv/write-csv writer dat-out))))

;(let [n-runs 5000
;      samples (sampler model/two-stage-poisson-query arg-map n-runs)]
;  (dashboard samples))
;(def n-samples 10000)
;(def samples (sampler model/two-stage-poisson-query arg-map n-samples))
;(util/write-lambdas! samples "lambdas.csv")
;(write-lambda-priors! arg-map n-samples "priors.csv")
;(get-in (first samples) [:result :lambda-1])
;(util/from-result (first samples) [:lambda-1])
;(util/write-seasons! samples #(get-in % [:result :lambda-2]) "outtest.txt")
;(oz/v! (util/dashboard-spec samples (:data arg-map)))
;(oz/v! (util/dashboard-spec samples ))
;(oz/v! (util/week-histo-spec samples 7))
(defn plot-results! [samples]
  (oz/start-plot-server!)
  (oz/v! (util/dashboard-spec samples)))

(def cli-opts
  [["-n" "--n-samples n-samples" "Number of program runs for influenza season"
    :default 100
    :parse-fn #(Integer/parseInt %)]
   ["-S" "--n-susceptible n-susceptible" "Population size"
    :default 100000
    :parse-fn #(Integer/parseInt %)]
   ["-I" "--n-infected n-infected" "Number of initially infected individuals"
    :default 2
    :parse-fn #(Integer/parseInt %)]
   ["-t" "--t-max t-max" "Number of weeks for season"
    :default 40
    :parse-fn #(Integer/parseInt %)]
   ["-p" "--n-particles n-particles" "Number of particles"
    :default 100
    :parse-fn #(Integer/parseInt %)]
   ["-o" "--outfile"]
   ["-d" "--data"]
   ["-g" "--plot-results"]
   ["-h" "--help"]])

(defn -main [& args]
  (let [
        ;; getting cli arguments
        parsed-opts (parse-opts args cli-opts)
        n-samples (get-in parsed-opts [:options :n-samples])
        n-susceptible (get-in parsed-opts [:options :n-susceptible])
        n-infected (get-in parsed-opts [:options :n-infected])
        t-max (get-in parsed-opts [:options :t-max])
        n-particles (get-in parsed-opts [:options :n-particles])

        arguments (-> arg-map
                      (assoc-in [:n-samples] n-samples)
                      (assoc-in [:inits :S] n-susceptible)
                      (assoc-in [:inits :I] n-infected)
                      (assoc-in [:t-max] t-max))

        ;; run model with or without data
        samples (if (get-in parsed-opts [:options :data])
                  (sampler model/two-stage-poisson-query (assoc-in arguments [:data] data) n-particles)
                  (sampler model/two-stage-poisson-query arguments n-particles))
        ]

    (if-let [outfile (get-in parsed-opts [:arguments 0])]
      (util/write-seasons-as-df! samples outfile))))

