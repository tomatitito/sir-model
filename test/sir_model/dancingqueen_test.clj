(ns sir-model.dancingqueen-test
  (:require
    [clojure.test :refer :all]
    [sir-model.dancingqueen :refer :all]
    [util.functions :as u])
  (:use
    [anglican [core :exclude [-main]] runtime emit]))

(defquery
  dancing-test-query [coll]
  (let
    [
     new (primary 0 coll)
     ]

    {:new new}
    ))

(deftest initialization
  (let [data-noinits (create-args-coll 10 [:S :I :R])
        sample-noinits (first (doquery :lmh dancing-test-query [data-noinits]))

        data-inits (create-args-coll 10 [:S :I :R] {:S 100 :I 42})
        sample-inits (first (doquery :lmh dancing-test-query [data-inits]))
        ]
    (testing
      (is (= (count (get-in sample-noinits [:result :new])) 10))
      (is (= (u/from-result sample-inits [:new 0 :S]) 100))
      (is (= (u/from-result sample-inits [:new 0 :I]) 42)))))
