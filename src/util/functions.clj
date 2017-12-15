(ns util.functions
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(defn from-season
  "Extracts value for key from a single sample. key must be in a result from
  an anglican query."
  [sample key]
  (reduce
    #(conj %1 (key %2))
    []
    (get-in sample [:result :season])))


(defn from-result
  "Extracts value for key from a single sample. key must be in a result from
  an anglican query. keys must be given in vector:
      (from-result a-sample [:a :b])"
  [sample keys]
  (get-in sample (flatten [:result keys])))


(defn from-results
  "Extracts values for key from samples. See from-result."
  [samples keys]
  (reduce
    #(conj %1 (from-result %2 keys))
    []
    samples))


(defn total-infected
  "Extracts number of infected individuals for a single sample from the Model."
  [sample]
  (from-season sample :I))


(defn new-infections
  [sample]
  (let [primary (from-season sample :primary)
        secondary (from-season sample :secondary)]
    (map #(+ %1 %2) primary secondary)))

;(def testsamples (new-infections (first sir-model.core/forced)))

;; Goal:
;; data-for-single-season should be able to return seqs of varying legths
;; so that it can either write
;; - values for one of the compartments (e.g. I) over the course of a season
;; - values for multiple or all of the compartments
;; - values that are computed from any combination of compartments (already possible!)

(defmulti #^{:private true} data-for-single-season (fn [cases sim-id] (sequential? (first cases))))

(defmethod data-for-single-season false [cases sim-id]
  (let
    [weeks (range (count cases))
     sim-ids (repeat (count cases) sim-id)]

    (partition 3
               (interleave weeks cases sim-ids))))

;; testing fn on small sample
(def new-I (new-infections (first sir-model.core/forced)))
(data-for-single-season new-I 0)





(defn write-seasons!
  [samples getter-fn outfile]
  (letfn
    [(csv-data [samples]
       (loop [coll []
              from-query samples
              n 0]

         (if (not (seq from-query))
           coll

           (let
             [cases (getter-fn (first from-query))
              csv-dat (data-for-single-season cases n)]

             (recur (apply conj coll csv-dat)
                    (rest from-query)
                    (inc n))))))]

    (with-open [writer (io/writer outfile)]
      (csv/write-csv writer (csv-data samples)))))

;; testing on small sample
(write-seasons! sir-model.core/forced new-infections "data/multi.csv")
