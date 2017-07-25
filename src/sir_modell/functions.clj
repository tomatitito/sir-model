(ns sir-modell.functions
  (:use [anglican core runtime emit]
        anglican-code.distributions
        anglican-code.queries
        util.functions))

;(defmulti ref-type-map (fn [keyw N] keyw)  )
;(defmethod ref-type-map :ref [keyw N]
;    (zipmap
;    (map #(keyword (str %)) (range N))
;    (repeatedly N #(ref 0)))
;   )

;(defn inc-actual-changes [ref]
;  " Increment the number of actual changes to a ref in I-counter-map. This will eventually
;  trigger the creation of a new thread that runs the cohort function. The reason is that a
;  watch is attached to every ref that counts the actual changes. The watch function will start
;  the new thread."
;  (update ref :actual inc) )

;(defn cohort-progression [current-infections recovery-rate I-map I-counter-map R-map current-timestep max-timesteps]
;  "Start the recovery progression in a cohort. For every timestep, a number of people
;  will recover. Those will be added to the number of recovered people at this timestep.
;  The number of infected people will be updated accordingly."
;  (if (< current-timestep max-timesteps)
;    (if (pos? current-infections)                                  ;are there still infectied people in this group
;
;      (let [cured (min current-infections (sample* (binomial current-infections recovery-rate)))
;            still-inf (- current-infections cured)
;            ref-in-I-map (get I-map (keyword (str current-timestep)))
;            ref-in-I-counter-map (get I-counter-map (keyword (str current-timestep)))
;            ref-in-R-map (get R-map (keyword (str current-timestep)))]
;
;        (dosync
;          (alter ref-in-I-map + still-inf)
;          (alter ref-in-I-counter-map inc-actual-changes)
;          (alter ref-in-R-map + cured))
;        ;(print-cur-inf-map cur-inf-map)
;        (cohort-progression still-inf recovery-rate I-map I-counter-map R-map (inc current-timestep) max-timesteps)))))


(defn compute-new-infections [I-map I-counter-map S-map R-map recovery-rate R-0 current-timestep max-timesteps]
  "Compute new infections for every timestep as a probabilistic function of
  current infections. Update number of infected and susceptible individuals
  accordingly."
  (let [current-infections (get I-map (keyword (str current-timestep)))]
    ;TODO: Update S-map
    (take 1 (doquery :smc new-infections-model [R-0 (deref current-infections) ]))))




(defn watch-test-fn [key watched old-state new-state]
  (let [touched (:actual new-state)
        needed (:required new-state)]
    ;(println new-state)
    (println (str key " old state: " old-state " new state: " new-state))))
