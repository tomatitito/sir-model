(ns sir-model.framework
  (:require [util.functions :as u]
            [sir-model.dataflow :as flow]
            [sir-model.two-stage-poisson :as model])
  (:use [anglican emit runtime [core :exclude [-main]]]))


(defm season-fn
      "Generic function for simulating an influenza season. Takes a starting-timestep,
      a collection of compartments and a lifetime-fn. timestep and collection are
      arguments for lifetime-fn. This of course means that lifetime-fn has to be a
      function that expects two arguments, the timestep and and collection."
      [t coll lifetime-fn]

      (let
        ;; before the actual simulation, progression for already
        ;; infected individuals at time 0 must be run once
        [initially-infected (get-in coll [0 :I])
         ;; a bit unfortunate, that model/progress is called like this
         ;; would be better to have a protocol with a progress method
         ;; then we could pass an instance of deftype/defprotocol instead of
         ;; lifetime-fn and call (.progress)
         initial-coll (model/progress 1 initially-infected coll)]


        (loop [t-cur t
               coll initial-coll]

          (if (= t-cur (count coll))
            coll

            (recur (inc t-cur)
                   (lifetime-fn t-cur coll))))))


