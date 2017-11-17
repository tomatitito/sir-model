(ns sir-model.cohort-test
  (:require
    [clojure.test :refer :all]
    [sir-model.cohort :refer :all] )
  (:use
    [anglican [core :exclude [-main]] runtime emit]))


;; first two tests moved from compartments_test
(with-test
  (def test-record (->SIR-Compartments 4 4 4))
  (is (= (:S test-record) 4))
  (is (= (:I test-record) 4))
  (is (= (:R test-record) 4)))


(with-test
  (def compartment-map (create-compartments-coll 3))
  (is (= (count compartment-map) 3)))


(defn sum-compartments
  "Sum specified compartments of an sir-record. Compartments have to be given as coll.
  Used to check, if e.g. the [:I :R] compartments sum to the same number over the course
  of a progression, as they should."
  ([sir-record comps]
   (sum-compartments sir-record 0 comps))
  ([sir-record acc comps]
   (loop [[head & tail] comps
          sum acc]
     (if (not (seq tail))
       (+ sum (get sir-record head))
       (recur tail (+ sum (get sir-record head)))))))


(defn compare-sums-over-time
  "Calls sum-compartments for every timestep of a progression and compares
  the results for equality."
  ([compartments-map comps]
    (let [match-val (get-in (first compartments-map) comps)
          bool-vec (map #(= (get-in % comps) match-val) compartments-map)]
      (reduce #(and %1 %2) bool-vec))))


(defquery sums-at-start [before-start]
          (let [compartments (start-cohort 0 0.2 0.5 before-start)]
            {:after-start compartments}))


(deftest sums-at-start-of-cohort
  (let [before-start (create-and-init-compartments-map 6 1000 100)
        samples (doquery :lmh sums-at-start [before-start])
        after-start (get-in (first samples) [:result :after-start])
        ]
    (testing
      (is (= (sum-compartments (before-start 0) [:S :I :R]) (sum-compartments (after-start 0) [:S :I :R])))
      )))


(defquery sums-over-time [initial-comps ]
          (let [
                before (start-cohort 0 0.2 0.5 initial-comps)
                after (progress 0 0.5 before)
                ]
            {:compartments after}))


(deftest sums-in-progression
  (let

    [init (create-and-init-compartments-map 30 10000 600)
     samples (doquery :lmh sums-over-time [init 10])
     compartments (get-in (first samples) [:result :compartments])]

    (testing "Check for equalitiy of compartment-sums over timesteps in a single cohort."
      (is (compare-sums-over-time compartments [:S]))
      (is (compare-sums-over-time compartments [:I :R]))
      (is (compare-sums-over-time compartments [:S :I :R])))))


(deftest sums-in-season
  (let
    [samples (doquery :lmh simple-poisson-process-model [10 cohort-progression 70000000 30])
     season (get-in (first samples) [:result :season])]
    (testing "Check if total number of individuals remains constant in a season."
      (is (compare-sums-over-time season [:S :I :R])))))

