(ns sir-modell.datastructures)


; Data structure that has as many elements as S, I and R. Every element is
; a ref to a map with two key-val pairs, one for actual number of changes
; to the ref itself and one for required number of changes so that new infecitons
; can be computed. Remember: For new infections at a give point in time to be
; computed, all other thread must have finished their cohort progressions for
; this timestep.
(defn create-I-counter [new-map size watch-fn]
  (if (= size 0) new-map

    (let [position (dec size)
          new-entry (ref {:actual 0 :required (max 0 (dec position))})
          counter-map (assoc new-map (keyword (str position)) new-entry)
          ;new-ref (keyword (str position) counter-map)
          ]
      (add-watch (get counter-map (keyword (str position))) (keyword (str "ci_" position)) watch-fn)
      (create-I-counter counter-map (dec size) watch-fn)))
  )


;NEED THIS?
;(defprotocol Compartment
;  "Represents a compartment in SIR-Model, which is a map with an entry for each timestep."
;  (update-value [this timestep updating-value]))
;
;
;(defrecord I-map [infections-per-timestep-map]
;  Compartment
;  (update-value [this timestep new-infections]
;    (let [current-infections (get infections-per-timestep-map (keyword (str timestep)))]
;      (+ current-infections new-infections))))

(defrecord cohort [S-map I-map I-counter-map R-map R-0 recovery-rate timestep])

