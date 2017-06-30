(ns anglican-code.queries
  (:use [anglican core emit runtime]
        anglican-code.distributions
        anglican-code.prob_functions))


(with-primitive-procedures
  [geometric new-infections]
  (defquery new-infections-model [R-0 current-infections & data]
            (let [
                  new (sample (new-infections R-0 current-infections))
                  R-0-est (sample (normal R-0 0.1))
                  ]
              ;(if data
              ;  (observe (binomial current-infectio<ns transmission-rv) data))
              {:new-infections new :R0 R-0-est})))