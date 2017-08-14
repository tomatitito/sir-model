(ns sir-model.cohort-test
  (:require
    [clojure.test :refer :all]
    [sir-model.cohort :refer :all]
    [sir-model.compartments :refer :all]))

(with-test
  (def init (create-and-init-compartments-map 7 1000 76))

  (def compartments (progress 1 4 50 (start-cohort 1 init 2) 0.4))

  (defn sum-compartments
    ([comps t]
     (+ (get-in comps [(keyword (str t)) :S]) (get-in comps [(keyword (str t)) :I]) (get-in comps [(keyword (str t)) :R]))) )


  (defn test-sum
    "Is sum of SIR-compartments for time t the same as for time t-1?"
    [comps t]
    (= (sum-compartments comps t) (sum-compartments comps (dec t))))


  (defn test-sums-recur
    "Recursively compare sums of SIR-compartments over all timesteps"
    ([comps] (test-sums-recur comps (count comps)))
    ([comps t]
     (if (= 2 t)
       (test-sum comps 2)
       (and (test-sum comps t) (test-sums-recur comps (dec t))))))

  (is (test-sums-recur compartments)) "Consistency of numbers in SIR-compartments")
