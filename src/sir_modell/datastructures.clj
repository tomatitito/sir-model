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


;; Add a watch to every element of the I-counter map. This is used to
;; check if new infections can be computed.
;(dotimes [i timesteps]
;  (let [watch-key (keyword (str "I-count-" i))
;        ref-in-I-counter ((keyword (str i)) I-counter)]
;    (add-watch ref-in-I-counter watch-key watch-test-fn)))
