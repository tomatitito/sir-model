(ns sir-model.dataflow)


(defn create-args-coll
  "Returns a datastructure holding the compartments for n timesteps. Takes an int, a vector of keywords
  and optionally a map of key value pairs. Returns a vector of n maps, where each map has all the the
  keywords in keys with inital value of zero. If inits is supplied, the first entry in the returned vector
  will associate the given values to the corresponding keys.

      (create-args-coll 10 [:S :I :R] {:S 10000 :I 100})"
  ([n keys]
   (let [m (zipmap keys (repeat (count keys) 0))]
     (into [] (repeat n m))))
  ([n keys inits]
   (let [tail (create-args-coll (dec n) keys)
         head (reduce #(assoc-in %1 [(key %2)] (val %2)) (first tail) inits)]
     (into [head] tail))))


(defn cohort-size
  "Number of individuals for cohort starting from time t."
  [t coll]
  (+
    (get-in coll [t :primary])
    (get-in coll [t :secondary])))


(defn a->b
  "Takes a map m and two vectors a and b with keys, substracts value from a and adds it to b."
  [a b value m]
  (-> m
      (update-in ,,, a #(- % value))
      (update-in ,,, b #(+ % value))))


(defn S->
  "Takes a map of compartments, subtracts value from :S at time t and adds it to to at time t."
  [t to value coll]
  (a->b [t :S] [t to] value coll))


(defn S->primary [t value coll]
  (a->b [t :S] [t :primary] value coll))


(defn S->secondary [t value coll]
  (a->b [t :S] [t :secondary] value coll))


(defn ->coll
  "Adds value to compartments coll. target is a keyseq for determining where to add value."
  [target value coll]
  (update-in coll target + value))


(defn ->compartments
  "Adds multiple values to corresponding compartments. args needs to be a sequence of vector
  nested keys and corresponding values. Returns updated coll with each value added to the
  corresponding compartment."
  [targets-and-vals coll]
  (let [pairs (partition 2 targets-and-vals)]
    (reduce #(->coll (first %2) (second %2) %1) coll pairs)))


(defn update-rules
  "Given the number of cases and the number of removed individuals, determines how the compartments need to be updated.
  Returns a vector of (nested) keyseqs and corresponding values, which is fed into ->compartments."
  [t cases removed]
  [[t :R] removed [t :I] (- cases removed)])


(defn ->I [t value coll]
  (->coll [t :I] value coll))


(defn ->R [t value coll]
  (->coll [t :R] value coll))




