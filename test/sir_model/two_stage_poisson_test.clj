(ns sir-model.two-stage-poisson-test
  (require [sir-model.two-stage-poisson :as model]
           [sir-model.dataflow :as flow]
           [util.functions :as u]
           [clojure.test :as t])
  (:use
    [anglican [core :exclude [-main]] runtime emit]))


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


(def args
  {:t-max        20
   :compartments [:S :I :R :primary :secondary]
   :inits        {:S 1000 :I 100}
   :prior-1      (uniform-continuous 0.2 0.5)
   :prior-2 (uniform-continuous 0.4 0.6)})


(with-primitive-procedures
  [flow/create-args-coll]
  (defquery
    test-query [args lifetime-fn]
    (let
      [compartments (create-args-coll (:t-max args) (:compartments args) (:inits args))
       r-1 (sample (:prior-1 args))
       r-2 (sample (:prior-2 args))

       ;; for testing if initially infected are handled correctly
       initial-only (model/progress 1 (get-in compartments [0 :I]) compartments)

       ;; lifetime-fn for simulation
       f #(lifetime-fn %1 r-1 r-2 %2)
       season (model/season-fn 0 compartments f)]

      {:initial-only initial-only :season season})))


(t/deftest inital-I-only
         (let
           [initial-only (u/from-result (first (doquery :lmh test-query [args model/form-and-prog])) [:initial-only])]
           (t/testing
             (t/is (compare-sums-over-time initial-only [:S :I :R])))))

(t/deftest simulation-run
  (let
    [season (u/from-result (first (doquery :lmh test-query [args model/form-and-prog])) [:season])]
    (t/testing
      (t/is (compare-sums-over-time season [:S :I :R :primary :secondary])))))
