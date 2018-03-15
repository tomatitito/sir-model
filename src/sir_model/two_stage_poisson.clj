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
  (defm start-and-progress
        "Simulating the lifetime of a cohort including formation and progression."
        [t l-1 l-2 coll]
        ((comp
           #(progress (inc t) (cohort-size t %) %)
           #(start-poisson-poisson t l-1 l-2 %))
          coll)))


(defm split-observed
      "Split number of observed cases according to ratio of primary and secondary cases
      combined with parameters l-1 and l-2. Returns a vector with numbers for estimated
      primary and secondary observed cases."
      [t l-1 l-2 coll args]
      (let
        [n-total (get (:data args) t)
         n-primary (get-in coll [t :primary])
         n-secondary (get-in coll [t :secondary])]

        (cond
          ;; if there are no primary infections, all new cases are
          ;; caused by secondary infections and vice versa
          (zero? n-primary) [0 n-total]
          (zero? n-secondary) [n-total 0]
          ;; if there are no infectious people at all but there is data,
          ;; split the data according to ratio of params
          (and (zero? n-primary) (zero? n-secondary)) [(* l-1 n-total) (* l-2 n-total)]
          :else (let
                  [ratio (/
                           (* l-2 n-secondary)
                           (+ (* l-2 n-secondary) (* l-1 n-primary)))
                   obs-2 (* ratio n-total)
                   obs-1 (- n-total obs-2)]
                  [obs-1 obs-2])
          )))


(defm cohort-lifetime
      [t l-1 l-2 coll args]
      (let
        [updated-coll (start-and-progress t l-1 l-2 coll)]

        (when (and
                (:data args) (< t (count (:data args))))
          (let
            [n-primary (get-in coll [t :primary])
             n-secondary (get-in coll [t :secondary])
             ;; estimating the proportion of new primary and secondary cases
             [cases-primary cases-secondary] (split-observed t l-1 l-2 coll args)]

            (observe (poisson (* l-1 n-primary)) cases-primary)
            (observe (poisson (* l-2 n-secondary)) cases-secondary)))
        updated-coll))


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

       ;f #(start-and-progress %1 lambda-1 lambda-2 %2)
       f #(cohort-lifetime %1 lambda-1 lambda-2 %2 args)
       ;dance (cohort-lifetime 0 lambda-1 lambda-2 initialized-comps args)

       season (fw/season-fn 0 compartments f)]


      {:lambda-1 lambda-1 :lambda-2 lambda-2 :season season})))


