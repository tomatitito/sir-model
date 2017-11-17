(ns sir-model.core-test
  (:require [clojure.test :refer :all]
            [sir-model.core :refer :all]
            [clojure.core.async :as as])
  (:use [anglican runtime [core :exclude [-main]] emit]
        anglican-code.distributions
        ))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 1 1))))

(defm mimic-start-cohort
      "Writes the number t to the t-th position in coll."
      [t coll]
      (assoc-in coll [t :I] t))

