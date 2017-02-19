(ns anglican-code.prob_functions
  (:use [anglican 
         [core :exclude [-main]]
         runtime
         emit 
         stat])
  )


;rekursiv die Anteile der infizierten und gesundeten Personen berechnen
(defm incidence-over-time [ [I R mean-duration dat-out maxtime] ]
  (if (= maxtime 0) dat-out
    (let 
      ;Anteil von gesundeten haengt mit durchschnittlicher Krankheitsdauer zusammen: 
      ;new-R = 1/mean-dur * I + old-R
      [ new-R ((fn [infected recovered] (+ recovered (* (/ 1 (+ (sample (poisson 4)) 1)) infected))) I R)] 
      (incidence-over-time [(max (- I new-R) 0) 
                            new-R 
                            mean-duration 
                            (conj dat-out new-R) 
                            (- maxtime 1)])
      )
    )
  )

;Aenderungsrate von R als 1/mittlere_Krankheitsdauer * I
(defm change-in-R [[I mean-dur]]
  (* (/ 1 mean-dur) I))
