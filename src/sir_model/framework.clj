(ns sir-model.framework
  (:require [util.functions :as u]
            [sir-model.dataflow :as flow])
  (:use [anglican emit runtime [core :exclude [-main]]]))



(with-primitive-procedures
  [flow/S->]
  (defm infect
        "Infections for time t. Infecting individuals from who infect new individuals as specified by how (must be a
        function). Returns updated-coll."
        [t who whom how coll]
        (let
          [max-cases (get-in coll [t :S])
           old-cases (get-in coll [t who])

           ;; there cannot be more new cases than susceptibles
           new-cases (if (pos? old-cases)
                       (min (how old-cases) max-cases)
                       0)]
          (S-> t whom new-cases coll))))



(with-primitive-procedures
  [flow/cohort-size]
  (defm season-fn
        "Generic function for simulating an influenza season. Takes a starting-timestep,
        a collection of compartments and a lifetime-fn. timestep and collection are
        arguments for lifetime-fn. This of course means that lifetime-fn has to be a
        function that expects two arguments, the timestep and and collection."
        [t coll lifetime-fn]

        (loop [t-cur t
               c coll]

          (if (= t-cur (count c))
            c

            (recur (inc t-cur)
                   (lifetime-fn t-cur c))))))
