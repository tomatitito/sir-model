(ns anglican-code.distributions
  (:use [anglican core emit runtime]))

(defn geometric-recursion [p acc]
  (if (sample* (flip p))
    acc
    (geometric-recursion p (inc acc))))

(defdist geometric
  "Geometric distribution. Returns the number of misses up to and not including
  the first success."
  [p] []
  (sample* [this] (geometric-recursion p 0))
  (observe* [this value]
    (let [unpacked (repeat value 0)]
     (first (map #(observe* (bernoulli p) %) unpacked)))))



(defdist new-infections
  "Distribution of new-infections. The number of new infections depends on the number of
  already infected people. An infected individual transmits the disease to some number of
  individuals. Parameters to this distribution are R0, the basic reproduction number and
  the number of already infected individuals (prevalence). R0 is conceptualized as a geometric
  random variable."
  [R0 already-infected] []
  (sample* [this] (reduce + (repeatedly already-infected #(sample* (geometric (/ 1 R0))))))
  (observe* [this value] (observe* (reduce + (repeatedly already-infected #(geometric (/ 1 R0)))) value)))


