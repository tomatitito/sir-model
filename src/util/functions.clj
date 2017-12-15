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

(defmulti #^{:private true} data-for-single-season (fn [query-result f sim-id] (sequential? f)))

(defmethod data-for-single-season false [query-result f sim-id]
  (let
    [weeks (range (count (from-result query-result :season)))
     cases (f query-result)
     sim-ids (repeat (count weeks) sim-id)]

    (partition 3
               (interleave weeks cases sim-ids))))

;;; testing fn on small sample
;(data-for-single-season (first sir-model.core/forced) new-infections 0)

(defmethod data-for-single-season true [query-result f sim-id]
  (let
    [weeks (range (count (from-result query-result :season)))
     sim-ids (repeat (count weeks) sim-id)
     cases (map #(%1 query-result) f)]

    (as-> cases v
          (apply interleave v)
          (partition (count cases) v)
          (interleave weeks v sim-ids)
          (flatten v)
          (partition (+ (count cases) 2) v))))

(data-for-single-season (first sir-model.core/forced) [new-infections total-infected] 42)



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
             [
              ;cases (getter-fn (first from-query))
              single-sample (first from-query)
              csv-dat (data-for-single-season single-sample getter-fn n)
              ]

             (recur (apply conj coll csv-dat)
                    (rest from-query)
                    (inc n))))))]

    (with-open [writer (io/writer outfile)]
      (csv/write-csv writer (csv-data samples)))))

;; testing on small sample
(write-seasons! sir-model.core/forced [new-infections total-infected] "data/multi.csv")
