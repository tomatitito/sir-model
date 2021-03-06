(ns sir-model.two-stage-poisson
  (:require [sir-model.distributions :as d]
            [sir-model.dataflow :as flow]
            [sir-model.framework :as fw])
  (:use [anglican [core :exclude [-main]] runtime emit]))



(defdist new-cases-dist
  "Custom distribution to sample new infections from N infectious individuals
  with parameter lambda."
  [N lambda]
  [*lambda (* N lambda)]
  (sample* [this]
           (cond
             (zero? *lambda) 0
             (> *lambda 30) (sample* (d/fast-poisson *lambda))
             :else (sample* (poisson *lambda))))
  (observe* [this value]
            (if (pos? N)
              (observe* (poisson *lambda) value)
              0)))

(defdist two-stage-poisson-dist
  "Custom distribution representing new cases in the two stage poisson model.
  Expects number of primary and secondary cases and respective parameters as
  inputs. sample returns a sample from this distribution. When given a number
  of observed new infections, observe computes the log-likelihood of given the
  number of infections. Wraps around fast-poisson and new-cases-dist."
  [N-1 l-1 N-2 l-2]
  [*l-1 (* N-1 l-1)
   *l-2 (* N-2 l-2)
   *lambda (+ *l-1 *l-2)]
  (sample* [this] (sample* (new-cases-dist 1 *lambda)))
  (observe* [this value]
            (if (pos? *lambda)
              (observe* (poisson *lambda) value)
              0)))


(with-primitive-procedures
  [new-cases-dist]
  (defm generate-poisson
        [N lambda]
        "Draws a sample from the distribution of new infections given parameter
         lambda and number of already infected individuals."
        (sample (new-cases-dist N lambda))))


(defm primary-poisson
      "Samples number of primary cases at time t from a poisson distribution.
      Primary cases are those that are generated by the already infected
      people, given as the value for key :I for the current timestep. Returns
      the map of compartments with updated number of primary cases for time t."
      [t lambda coll]
      (fw/infect t :I :primary #(generate-poisson % lambda) coll))


(defm secondary-poisson
      "Samples the number of secondary cases at time t based on the number of
       primary cases. See primary-poisson."
      [t lambda coll]
      (fw/infect t :primary :secondary #(generate-poisson % lambda) coll))


(with-primitive-procedures
  [flow/a->b]
  (defm remove-vaccinated
        "Because some people are vaccinated, they will not get sick after
         encountering a sick person. To account for this, some of those that
         have been infected are now removed from the primary and secondary
         compartemnts and are put back into the S compartment."
        [t vac-rate coll]
        (let
          [rem-pri (sample (binomial (get-in coll [t :primary]) vac-rate))
           rem-sec (sample (binomial (get-in coll [t :secondary]) vac-rate))]

          (a->b [t :primary] [t :S] rem-pri
                     (a->b [t :secondary] [t :S] rem-sec coll)))))



(defm start-poisson-poisson
      "Start a cohort in two phases. First phase uses primary-poisson, second
      phase uses secondary-poisson. Is really just a convenience wrapper around
      those functions."
      [t l-1 l-2 coll]
      ((comp
         #(remove-vaccinated t 0.2 %)
         #(secondary-poisson t l-2 %)
         #(primary-poisson t l-1 %))
       coll))


(defm update-in-ang
  "'Updates' a value in a nested associative structure, where ks is a sequence
   of keys and f is a function that will take the old value and any supplied
   args and return the new value, and returns a new nested structure.  If any
   levels do not exist, hash-maps will be created."
  [m ks f & args]
  (let [up (fn up [m ks f args]
             (let [[k & ks] ks]
               (if ks
                 (assoc m k (up (get m k) ks f args))
                 (assoc m k (apply f (get m k) args)))))]
    (up m ks f args)))


(defm progress
  [t cases coll]

  (if (= t (count coll))
    ;; if time's up, remaining cases
    ;; are discarded
    coll

    (let
      [removed (sample (binomial cases 0.45))
       remaining (- cases removed)
       susceptible (get-in coll [(dec t) :S])

       ;; during progression a number of things need to happen at each timestep
       ;; some individuals recover, their number has to be added to [t :R]
       ;; the rest remains infected, this number has to be added to [t :I]
       ;; the number of susceptibles has to be copied from (dec t) to t
       updated-coll (assoc-in
                      (update-in-ang
                        (update-in-ang coll [t :R] + removed)
                        [t :I] + remaining)
                      [t :S] susceptible)]

      (progress (inc t) remaining updated-coll))))


(defm init-compartments
      "Before the actual simulation, progression for already infected
      individuals at time 0 must be run once. Neccessary for correctness of the
      number of individuals over the course of the simulation."
      [coll]
      (let
        [initially-infected (get-in coll [0 :I])]
        (progress 1 initially-infected coll)))


(with-primitive-procedures
  [flow/cohort-size]
  (defm start-and-progress
        "Simulate formation and progression of a cohort."
        [t l-1 l-2 coll]
        ((comp
           #(progress (inc t) (cohort-size t %) %)
           #(start-poisson-poisson t l-1 l-2 %))
         coll)))


(with-primitive-procedures [two-stage-poisson-dist flow/cohort-size flow/S-> flow/a->b]
  (defm start-observe-progress
    "In case data is available, observe on it after start and before
    progression of a cohort."
    [t l-1 l-2 coll datapoint]
    (let
      [
       ;primary (generate-poisson datapoint l-1)
       primary (sample (poisson (* datapoint l-1)))
       secondary (generate-poisson primary l-2)

       ;; updating coll
       ;; primary and secondary cases have to be subracted from :S
       ;; and added to :primary and :secondary
       ;temp (S-> t :primary primary coll)
       temp (a->b [t :S] [t :primary] primary coll)
       ;updated-coll (S-> t :secondary secondary temp)
       updated-coll (a->b [t :S] [t :secondary] secondary temp)]

      (if (zero? t)
        ;; start of simulation, no observing
        (progress (inc t) (cohort-size t updated-coll) updated-coll)
        ;; else observe
        ;; how likely is the datapoint to have been generated by the primary
        ;; and secondary cases from the previous timestep?
        (let [prev-primary (get-in updated-coll [(dec t) :primary])
              prev-secondary (get-in updated-coll [(dec t) :secondary])]
          (observe
            (two-stage-poisson-dist prev-primary l-1 prev-secondary l-2)
            datapoint)
          (progress (inc t) (cohort-size t updated-coll) updated-coll))))))


(with-primitive-procedures
  [new-cases-dist]
  (defm cohort-lifetime
    "Represent a cohort through its entire lifetime. Take care of start and
    progress and, if data is available, of observing the parameters. This
    function needs to be passed to the season-fn to be recursively called for
    as many timesteps as needed."
    [t l-1 l-2 coll args]
    (if (and
          (:data args) (< t (count (:data args))))
      (start-observe-progress t l-1 l-2 coll (get (:data args) t))
      (start-and-progress t l-1 l-2 coll))))


(with-primitive-procedures
  [flow/create-args-coll flow/cohort-size]
  (defquery
    two-stage-poisson-query
    [args]

    (let
      [compartments (create-args-coll
                      (:t-max args)
                      (:compartments args)
                      (:inits args))
       initialized-comps (init-compartments compartments)

       lambda-1 (sample (:prior-1 args))
       lambda-2 (sample (:prior-2 args))

       ;f #(start-and-progress %1 lambda-1 lambda-2 %2)
       f #(cohort-lifetime %1 lambda-1 lambda-2 %2 args)

       season (fw/season-fn 0 compartments f)]


      {:lambda-1 lambda-1 :lambda-2 lambda-2 :season season})))


