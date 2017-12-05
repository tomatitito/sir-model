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



(defn write-seasons!
  [samples getter-fn outfile]
  (letfn
    [(data-for-single-season [cases sim-id]
       (let
         [weeks (range (count cases))
          sim-ids (repeat (count cases) sim-id)]

         (partition 3
                    (interleave weeks cases sim-ids))))

     (csv-data [samples]
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
