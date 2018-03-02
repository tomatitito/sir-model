(ns sir-model.two-stage-poisson
  (:require [anglican-code.distributions :as d]
            [sir-model.dataflow :as flow]
            [sir-model.framework :as fw])
  (:use [anglican [core :exclude [-main]] runtime emit]))


(with-primitive-procedures
  [d/fast-poisson]
  (defm generate-poisson
        [N lambda]
        "Draws a sample from the distribution of new infections given parameter lambda and number of already
        infected individuals."
        (let [lambda* (* N lambda)]
          (if (> lambda* 30)
            (sample (fast-poisson lambda*))
            (sample (poisson lambda*))))))


(defm primary-poisson
      "Samples number of primary cases at time t from a poisson distribution. Primary cases are those that are
      generated by the already infected people, given as the value for key :I for the current timestep. Returns
      the map of compartments with updated number of primary cases for time t."
      [t lambda coll]
      (fw/infect t :I :primary #(generate-poisson % lambda) coll))


(defm secondary-poisson
      "Samples the number of secondary cases at time t based on the number of primary cases. See primary-poisson."
      [t lambda coll]
      (fw/infect t :primary :secondary #(generate-poisson % lambda) coll))


(defm start-poisson-poisson
      "Starts a cohort in two phases. First phase uses primary-poisson, second phase uses secondary-poisson. Is really
      just a convenience wrapper around those functions."
      [t l-1 l-2 coll]
      ((comp
         #(secondary-poisson t l-2 %)
         #(primary-poisson t l-1 %))
        coll))


(with-primitive-procedures
  [flow/update-rules flow/->compartments]
  (defm progress
        [t cases coll]

        (if (= t (count coll))
          ;; if time's up, remaining cases
          ;; are discarded
          coll

          (let
            [removed (sample (binomial cases 0.45))
             remaining (- cases removed)

             ;; update-rules returns a vector that has all
             ;; information about how coll is updated
             where-and-what (update-rules t cases removed)

             ;; in addition to removed and remaining cases,
             ;; the number of susceptibles must be retained
             ;; throughout the progression
             updated-1 (->compartments where-and-what coll)
             updated-coll (assoc-in updated-1 [t :S] (get-in updated-1 [(dec t) :S]))]

            (progress (inc t) remaining updated-coll)))))


(defm init-compartments
      "Before the actual simulation, progression for already infected individuals at time 0 must be run once.
      Neccessary for correctness of the number of individuals over the course of the simulation."
      [coll]
      (let
        [initially-infected (get-in coll [0 :I])]
        (progress 1 initially-infected coll)))


(with-primitive-procedures
  [flow/cohort-size]
  (defm form-and-prog
        "Formation and Progression of a cohort."
        [t l-1 l-2 coll]
        ((comp
           #(progress (inc t) (cohort-size t %) %)
           #(start-poisson-poisson t l-1 l-2 %))
          coll)))


(with-primitive-procedures
  [flow/cohort-size]
  (defm cohort-lifetime
        "Simulating the lifetime of a cohort including formation and progression."
        [t l-1 l-2 coll]
        ((comp
           #(progress (inc t) (cohort-size t %) %)
           #(start-poisson-poisson t l-1 l-2 %))
          coll)))


(with-primitive-procedures
  [flow/create-args-coll flow/cohort-size]
  (defquery
    two-stage-poisson-query
    [args]

    (let
      [compartments (create-args-coll (:t-max args) (:compartments args) (:inits args))
       initialized-comps (init-compartments compartments)

       lambda-1 (sample (:prior-1 args))
       lambda-2 (sample (:prior-2 args))

       f #(cohort-lifetime %1 lambda-1 lambda-2 %2)

       season (fw/season-fn 0 compartments f)]


      {:season season})))

