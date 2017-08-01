(ns sir-model.compartments-test
  (:require [clojure.test :refer :all]
            [sir-model.compartments :refer :all]))

(with-test
  (def test-record (->SIR-Compartments 4 4 4))
  (is (= (:S test-record) 4))
  (is (= (:I test-record) 4))
  (is (= (:R test-record) 4)))


(with-test
  (def compartment-map (create-compartments-map 3))
  (is (= (count compartment-map) 3)))

(with-test
  (def test-record (->SIR-Compartments 0 0 0))
  (is (= (- 42) (:S (update-S 42 test-record))))
  (is (= 42 (:I (update-I 42 test-record))))
  (is (= 42 (:R (update-R 42 test-record)))))


(with-test
  (def test-record (->SIR-Compartments 0 0 0))
  (is (= (->SIR-Compartments 0 42 43) (update-IR 42 43 test-record)))
  )