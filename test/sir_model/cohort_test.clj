(ns sir-model.cohort-test
  (:require
    [clojure.test :refer :all]
    [sir-model.cohort :refer :all]
    [sir-model.compartments :refer :all]))




(defn sum-compartments
  ([comps t]
   (+ (get-in comps [(keyword (str t)) :S]) (get-in comps [(keyword (str t)) :I]) (get-in comps [(keyword (str t)) :R]))))


(defn test-sums
  "Recursively compare sums of SIR-compartments over all timesteps"
  ([comps] (test-sums comps 1))
  ([comps t]
   (letfn

     ;"Is sum of SIR-compartments for time t the same as for time t+1?"
     [(test-sum
        [comps t]
        (= (sum-compartments comps t) (sum-compartments comps (inc t))))
      ]

     (if (= t (count comps))
       true                                       ; is there a better way to end the recursion?
       (and (test-sum comps t) (test-sums comps (inc t)))))))


(deftest sums-in-progression
  (let

    [init (create-and-init-compartments-map 7 1000 76)
     compartments (progress 1 4 50 (start-cohort 1 init 2) 0.4)]

    (testing "check for equalitiy of compartment-sums over timesteps"
      (is (test-sums compartments 1)))))
