(ns sir-model.datastructures
  (:use [anglican [core :exclude [-main]] runtime emit]
        anglican-code.queries))


(defn create-watched-I-counter
  "Data structure that has as many elements as S, I and R. Every element is
  a ref to a map with two key-val pairs, one for actual number of changes
  to the ref itself, and one for required number of changes so that new infections
  can be computed and a new cohort can be cocnstructed. watch-fn is supposed to
  compare actual and required changes and call the constructor for the new cohort.
  Remember: For new infections at a give point in time to be computed, all other
  thread must have finished their cohort progressions for this timestep."
  [new-map size watch-fn]
  (if (= size 0)
    new-map

    (let
      [position (dec size)
       new-entry (ref {:actual 0 :required (inc position)})
       counter-map (assoc new-map (keyword (str position)) new-entry)]
      ;add watch
      (add-watch (get counter-map (keyword (str position))) (keyword (str "ci_" position)) watch-fn)
      ;recursively build up map
      (create-watched-I-counter counter-map (dec size) watch-fn))))


(defn inc-actual-changes [ref]
  "Increment the number of actual changes to a ref in a watched I-counter-map (as returned
  by create-watched-I-counter). This will eventually trigger the creation of a new thread that
  runs the cohort function. The reason is that a watch is attached to every ref that counts the
  actual changes. The watch function will start the new thread."
  (update ref :actual inc) )


;(defn watch-I-counter [key watched old-state new-state R-0]
;  "Watch function for watched-I-counter. Checks if all threads have made their
;  changes to the number of current infections. If so, constructor for a new cohort
;  is called and progression is started."
;  (let
;    [touched (:actual new-state)
;     needed (:required new-state)]
;    (if (= touched needed)
;      (do
;        (println "Es ist soweit: " (str key))
;        (take 1 (doquery :smc new-infections-model [R-0 (deref (keyword (str needed)) I-map)]) )))))


(defprotocol Progression
  "An abstraction for a cohort in SIR-Model. What is actually progressing is the number of
  infected individuals in the cohort. At the start of the progression, current-infections
  is the number of new infections for a given timepoint. During the progression individuals
  recover (or die), so the number gets smaller."
  (progress [ this current-timestep ]))

(defrecord cohort [current-infections S-map I-map I-counter-map R-map R-0 recovery-rate max-timesteps]
  Progression
  (progress [ this current-timestep]
    (if (< current-timestep max-timesteps)
      (if (pos? current-infections)                         ;are there still infectied people in this group

        (let [cured (min current-infections (sample* (binomial current-infections recovery-rate)))
              still-inf (- current-infections cured)
              ref-in-I-map (get I-map (keyword (str current-timestep)))
              ref-in-I-counter-map (get I-counter-map (keyword (str current-timestep)))
              ref-in-R-map (get R-map (keyword (str current-timestep)))]

          (dosync
            (alter ref-in-I-map + still-inf)
            (alter ref-in-I-counter-map inc-actual-changes)
            (alter ref-in-R-map + cured))
          ;(print-cur-inf-map cur-inf-map)
          (progress this (inc current-timestep)))))))

