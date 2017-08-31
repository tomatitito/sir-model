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

