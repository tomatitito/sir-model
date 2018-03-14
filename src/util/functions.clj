(ns util.functions
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(defn from-maps
  "Extracts all vals for key(s) ks from a coll of maps. ks must be given as
   a vector. Returns a vector."
  [coll ks]
  (reduce #(conj %1 (get-in %2 ks)) [] coll))


(defn from-season
  "Extracts value for key from a single sample. key must be in a result from
  an anglican query."
  [sample key]
  (reduce
    #(conj %1 (key %2))
    []
    (get-in sample [:result :season])))


(defn from-result
  "Extracts value for key from a single sample. key must be in a result from
  an anglican query. keys must be given in vector:
      (from-result a-sample [:a :b])"
  [sample keys]
  (get-in sample (flatten [:result keys])))


(defn from-results
  "Extracts values for key from samples. See from-result."
  [samples keys]
  (reduce
    #(conj %1 (from-result %2 keys))
    []
    samples))


(defn total-infected
  "Extracts number of infected individuals for a single sample from the Model."
  [sample]
  (from-season sample :I))


(defn new-infections
  [sample]
  (let [primary (from-season sample :primary)
        secondary (from-season sample :secondary)]
    (map #(+ %1 %2) primary secondary)))


(defmulti #^{:private true} data-for-single-season (fn [query-result f sim-id] (sequential? f)))

(defmethod data-for-single-season false [query-result f sim-id]
  (let
    [weeks (range (count (from-result query-result :season)))
     cases (f query-result)
     sim-ids (repeat (count weeks) sim-id)]

    (partition 3
               (interleave weeks cases sim-ids))))


(defmethod data-for-single-season true [query-result f sim-id]
  (let
    [weeks (range (count (from-result query-result :season)))
     sim-ids (repeat (count weeks) sim-id)
     cases (map #(%1 query-result) f)]

    (as-> cases v
          (apply interleave v)
          (partition (count cases) v)
          (interleave weeks v sim-ids)
          (flatten v)
          (partition (+ (count cases) 2) v))))


(defn write-seasons!
  [samples getter-fn outfile & header]
  (letfn
    [(csv-data [samples]
       (loop [coll []
              from-query samples
              n 0]

         (if (not (seq from-query))
           coll

           (let
             [single-sample (first from-query)
              csv-dat (data-for-single-season single-sample getter-fn n)]

             (recur (apply conj coll csv-dat)
                    (rest from-query)
                    (inc n))))))]

    (with-open [writer (io/writer outfile)]
      (when header
        (csv/write-csv writer header))
      (csv/write-csv writer (csv-data samples)))))


(defn vec->vega-time-series
  "Converts a vector of values. Returns a vector of maps, which have two key-value pairs,
  one for :week and one for :data (cases). This format can be supplied as a values vector
  for vega-lite."
  [vec]
  (letfn [(vec->series
            [v]
            (let [steps (range (count v))
                  steps-and-vals (zipmap steps v)]
              (for [[t v] steps-and-vals]
                {:week t :data v}))
            )
          ]
    (flatten
      (map
        #(vec->series %)
        vec))))


(defn extract-for-vega
  "Extracts data for kw from seasons in an anglican sample and converts to a format for
  plotting with vega-lite. Works only for single values, not e.g. for seasons."
  [samples kw]
  (->
    (from-seasons samples kw)
    (vec->vega-time-series)))


(defn weekly-plot-spec
  "Return a spec for vega-lite to plot data for kw in samples by week."
  [samples kw]
  {:data     {:values (util.functions/extract-for-vega samples kw)}
   :mark     "tick"
   :encoding {:x {:field "week"
                  :type "nominal"
                  }
              :y {:field "data"
                  :type  "quantitative"}}})


(defn get-weeks-from-samples
  "Takes anglican samples and returns only those for specified week."
  [samples week]
  (let
    [seasons-by-week (util/->vega-time-series (util/from-results samples [:season]))
     week-only (filter #(= week (get % :week)) seasons-by-week)]
    week-only))


