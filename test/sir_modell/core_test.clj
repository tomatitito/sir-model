(ns sir-modell.core-test
  (:require [clojure.test :refer :all]
            [sir-modell.core :refer :all])
  (:use [anglican runtime [core :exclude [-main]] emit]
        anglican-code.distributions))

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
  ;(letfn [(geometric-recursion-tuple
  ;          "Sample from geometric distribution and collect results in vector"
  ;          [p acc]
  ;          (let [res (sample* (flip p))]
  ;            (if res
  ;              (conj acc true)
  ;              (geometric-recursion-tuple p (conj acc res)))))
          ;(trials-until-success
          ;  "Count trials including first success from vector as returned by geometric-recursion-tuple"
          ;  [outcome-vec]
          ;  (count outcome-vec))
          ;]
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
