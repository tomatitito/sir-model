(ns sir-modell.core-test
  (:require [clojure.test :refer :all]
            [sir-modell.core :refer :all]
            [clojure.core.async :as as])
  (:use [anglican runtime [core :exclude [-main]] emit]
        anglican-code.distributions
        ))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))


(defn geometric-recursion-tuple [p acc]
  "Sample from geometric distribution and collect results in vector"
  (let [res (sample* (flip p))]
    (if res
      (conj acc true)
      (geometric-recursion-tuple p (conj acc res)))))

(defn trials-until-success [outcome-vec]
  "Count trials including first success from vector as returned by geometric-recursion-tuple"
  (let [trials-until-success (count outcome-vec)]
    trials-until-success))

(defn run-geometric [p N acc]
  "Wrapper around geometric-recursion-tuple and trials-until-success. Samples N times
  from geometric distribution and returns a vector of length N with each entry being
  the number of trials up to and including the first success."
    (if (= N 0)
      acc
      (run-geometric p (dec N) (conj acc (trials-until-success (geometric-recursion-tuple p []))))))

(defn avg [coll]
  "Calculate average for given collection"
  (let [sum (reduce + 0 coll)]
    (/ sum (count coll))))

; N times:
; geometric-recursion-tuple -> trials-until-success -> run-geometric -> avg
; compare against expected value

(deftest test-geometric
  (let [p 0.3
        N 1000
        actual (avg (run-geometric p N []))
        expected (/ 1 p)]
    (is (= (float actual) expected))
    )
  )

(deftest test-update-S
  (testing "update-S..."
    (let [s (sir-modell.cohort/->SIR-Compartments 0 (sir-modell.cohort/->I-compartment 0 3 (as/chan)) 0)
          x 42]
      (is (= (- x) (:S (sir-modell.cohort/update-S x s)))))))

(deftest test-update-I
    (let [timesteps 3
          already-inf 100
          still-inf 20
          s (sir-modell.cohort/->SIR-Compartments 0 (sir-modell.cohort/->I-compartment already-inf 0 (as/chan)) timesteps)
          ]
      (testing "update-I..."
        (is
          (= (+ already-inf still-inf) (get-in (sir-modell.cohort/update-I still-inf s) [:I :infected]))
          (= (dec timesteps) (get-in (sir-modell.cohort/update-I still-inf s) [:I :countdown]))
          ))))
