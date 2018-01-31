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


; In Anglican werden die Implementierungen zum Ziehen von Zufallszahlen aus einer gegebenen Verteilung des Apache
; Commons Math Projekts verwendet. Im Fall der Poisson-Verteilung nimmt die dafür beanspruchte Zeit mit der Größe
; des Parameter der Verteilung linear zu. Da dieser Parameter für das SIR-Modell proportional zur Größe der
; betrachteten Population ist, führt sie zu sehr langen Programmlaufzeiten. Eine alternative Implementierung ist
; daher wünschenswert. Im folgenden wird dazu ein Algorithmus implementiert, der von Atkinson (1979) beschrieben wurde.

(defn propose [lambda b a]
  (let
    [helper (fn [b a]
              (let [u (sample* (uniform-continuous 0 1))]
                (/
                  (-
                    a
                    (/
                      (log (- 1.0 u))
                      u))
                  b)))]

    (loop [x (helper b a)]
      (if (>= (floor (+ x 0.5)) 0)
        x
        (recur (helper b a))))))


(defn fast-poisson [lambda]
  (let
    [;; helper functions
     beta-f (fn [lambda]
              (/
                Math/PI
                (sqrt (* 3.0 lambda))))
     alpha-f (fn [lambda b]
               (* lambda b))

     ;; constants
     b (beta-f lambda)
     a (alpha-f lambda b)
     c (- 0.767 (/ 3.36 lambda))
     k (- (- (log c) lambda) (log b))

     ;; stage 1: proposal
     ;; move this into a loop/recur block
     x (propose lambda b a)
     n (floor (+ x 0.5))

     ;; stage 2: functions for rejection sampling
     lhs (fn [x]
           (let
             [v (sample* (uniform-continuous 0 1))
              y (- a (* b x))]
             (+
               y
               (log
                 (/
                   v
                   (pow (+ 1.0 (exp y)) 2))))))

     rhs (fn [n]
           (-
             (+ k (* n (log lambda)))
             (log-gamma-fn (inc n))))
     ]

    (loop [x (propose lambda b a)]
      (if (<= (lhs x) (rhs (floor (+ x 0.5))))
        (floor (+ x 0.5))
        (recur (propose lambda b a)))
      )))
