(ns sir-model.cohort-test
  (:require
    [clojure.test :refer :all]
    [sir-model.cohort :refer :all]
    [sir-model.compartments :refer :all]))


(defn sum-compartments
  "Sum specified compartments of an sir-record. Compartments have to be given as coll."
  ([sir-record comps]
   (sum-compartments sir-record 0 comps))
  ([sir-record acc comps]
   (loop [[head & tail] comps
          sum acc]
     (if (not (seq tail))
       (+ sum (get sir-record head))
       (recur tail (+ sum (get sir-record head)))))))


(defn compare-sums-over-time
  "Compares the sums of specified compartments pairwise for adjacent timesteps. Returns true, if all sums are equal."
  ([compartments-map comps]
   (compare-sums-over-time compartments-map 1 comps))
  ([compartments-map t comps]
   (if (= t (count compartments-map))
     true
     (and (= (sum-compartments (get compartments-map (keyword (str t))) comps)
             (sum-compartments (get compartments-map (keyword (str (inc t)))) comps))
          (compare-sums-over-time compartments-map (inc t) comps)))) )


(deftest sums-at-start-of-cohort
  (let [before-start (create-and-init-compartments-map 6 1000 100)
        after-start (start-cohort 1 3 before-start)]
    (testing
      (is (= (sum-compartments (:1 before-start) [:S :I :R]) (sum-compartments (:1 after-start) [:S :I :R])))
      ))
  )


(deftest sums-in-progression
  (let

    [init (create-and-init-compartments-map 30 10000 600)
     compartments (progress 1 (count init) 0.4 (start-cohort 1 2 init ))]

    (testing "check for equalitiy of compartment-sums over timesteps"
      ;(is (test-sums compartments ))
      (is (compare-sums-over-time compartments [:S]))
      (is (compare-sums-over-time compartments [:I :R]))
      (is (compare-sums-over-time compartments [:S :I :R]))
      )))
