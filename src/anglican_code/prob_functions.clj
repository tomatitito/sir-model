(ns anglican-code.prob_functions
  (:use [anglican
         [core :exclude [-main]]
         runtime
         emit
         stat]))



;rekursiv die Anteile der infizierten und gesundeten Personen berechnen
(defm incidence-over-time [ [I R mean-duration dat-out maxtime]]
  (if (= maxtime 0) dat-out
    (let
      ;Anteil von gesundeten haengt mit durchschnittlicher Krankheitsdauer zusammen:
      ;new-R = 1/mean-dur * I + old-R
      [ new-R ((fn [infected recovered] (+ recovered (* (/ 1 (+ (sample (poisson 4)) 1)) infected))) I R)]
      (incidence-over-time [(max (- I new-R) 0)
                            new-R
                            mean-duration
                            (conj dat-out new-R)
                            (- maxtime 1)]))))




;Aenderungsrate von R als 1/mittlere_Krankheitsdauer * I
(defm change-in-R [[I mean-dur]]
  (* (/ 1 mean-dur) I))

;---------------------------------------------------NEUER VERSUCH----------------------------------------

(defm N2S
  "Represents the number of suceptibles in a given Population of size N.
  It is coneptualized as a binomially distributed random variable with success
  or better hit rate p."
  [[N p]]
  (sample (binomial N p)))


(defm negative-binomial
  "Negative binomial distribution for number of trials until r-th success."
  [[r p trials hits]]
  (if
    (= hits r) trials
    (negative-binomial [r p (inc trials) (+ hits (sample (bernoulli p)))])))



(defdist location [pub-preference] []
  (sample* [this] (if (sample* (flip pub-preference)) :pub :starbucks))
  (observe* [this value] (observe* (flip pub-preference) (= value :pub))))



;ALTERNATIVE IMPLEMENTATION?
(defm encounters
  "Infected individuals don't encounter all susceptibles. The number of susceptibles
  that encounter infected indiviuals is conceptualized as a negative binomially distributed
  random variable with parameters r and p."
  [[ r p]]
  (negative-binomial [r p 0 0]))

; (defm encounters
  ; "Susceptible individuals don't neccesarily encounter infected individuals. The number
  ; of encounters is conceptualized as a negative-binomially distributed variable (waiting
  ; for r-th success)."
  ; [[susceptible proportion-infected]]
  ; (sample (negative-binomial susceptible proportion-infected)))


(defm S2I
  "Computes number of susceptibles who become infected (for a given timestep which is
  not yet coded into the model). So this is incidence. Conceptualized as a binomially
  distributed random variable with parameters for number of susceptibles and transmission
  rate"
  [[susceptibles transmission-rate]]
  (sample (binomial susceptibles transmission-rate)))


(defm multinom-recursive [[n prob-vector out-vector]]
  "Takes the size of sample, a list of probabilities and an empty vector. Returns that
  vector with a sample from corresponding multinomial distribution. Probabilities have
  to sum to one."
  (if (empty? (rest prob-vector))
    (conj out-vector n)
    (let [hits (sample (binomial n (first prob-vector)))]
      (multinom-recursive [(- n hits) (rest prob-vector) (conj out-vector hits)]))))


(defm multinomial [[n probabilities]]
  "Wrapper around multinom-recursive."
   (multinom-recursive [n probabilities []]))



(defm I2D
"With each timestep some if the infected will die.")

(defm I2I
  "With each timestep some of the infected will stay infected.")

