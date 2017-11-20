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


(defn total-infected
  "Extracts number of infected individuals for a single sample from the Model."
  [sample]
  (from-season sample :I))


(defn write-seasons!
  [samples n-runs outfile]
  (letfn
    [(csv-for-single-season [cases sim-id]
       (let [weeks (range (count cases))
             sim-ids (repeat (count cases) sim-id)]
         (partition 3
                    (interleave weeks cases sim-ids))))

     (build-csvs [samples n-runs]
       (loop [coll []
              from-query samples
              n 0]
         (if (= n n-runs)
           coll

           (let [cases (total-infected (first from-query))
                 csv-dat (csv-for-single-season cases n)]
             (recur (apply conj coll csv-dat)
                    (rest from-query)
                    (inc n))))))

     ]
    ;(build-csvs samples n-weeks)
    (with-open [writer (io/writer outfile)]
      (csv/write-csv writer (build-csvs samples n-runs)))))
