(ns sir-model.framework
  (:require [util.functions :as u]
            [sir-model.dataflow :as flow])
  (:use [anglican emit runtime [core :exclude [-main]]]))

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
