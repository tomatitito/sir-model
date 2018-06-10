(ns sir-model.util
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


(defn from-seasons
  "Extracts value for key from seasons. See from-season."
  [samples key]
  (reduce
    #(conj %1 (from-season %2 key))
    []
    samples))


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


(defn new-infections-in-season
  [sample]
  (let [primary (from-season sample :primary)
        secondary (from-season sample :secondary)]
    (map #(+ %1 %2) primary secondary)))


(defn new-infections-in-seasons
  [samples]
  (reduce #(conj %1 (new-infections-in-season %2)) [] samples))


(defn sum-compartments
  "Sum specified compartments of an sir-record. Compartments have to be given as coll.
  Used to check, if e.g. the [:I :R] compartments sum to the same number over the course
  of a progression, as they should."
  ([sir-record comps]
   (sum-compartments sir-record 0 comps))
  ([sir-record acc comps]
   (loop [[head & tail] comps
          sum acc]
     (if (not (seq tail))
       (+ sum (get sir-record head))
       (recur tail (+ sum (get sir-record head)))))))


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
  "Takes output generated from anglican and writes data for seasons generated
  by getter-fn to outfile. An additional header can be specified. getter-fn can
  be a single function or a vector of functions, which can be used to simply
  collect data from the output or to compute values based on them. Note that
  these functions have to operate on a single sample, since they are called
  recursively inside a loop."
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


(defn write-seasons-as-df!
  "Write simulation results to be read in as dataframe and plotted by R.
  Wraps around write-seasons!. outfile is given without file extension
  and automatically stored as csv-file."
  [samples outfile]
  (let
    [getter-fns [new-infections-in-season
                 #(from-season % :S)
                 #(from-season % :I)
                 #(from-season % :R)
                 #(from-season % :primary)
                 #(from-season % :secondary)]
     filedir "data"
     path (str filedir "/" outfile ".csv")
     header ["week" "new" "S" "I" "R" "primary" "secondary" "sim_id"]]

    (write-seasons! samples getter-fns path header)))


(defn write-lambdas!
  "Write l-1 and l-2 to csv."
  [samples outfile]
  (let [l-1 (from-results samples [:lambda-1])
        l-2 (from-results samples [:lambda-2])
        dat-out (->>
                  (interleave l-1 l-2)
                  (partition 2))]
    (with-open [writer (io/writer outfile)]
      (csv/write-csv writer [["lambda-1" "lambda-2"]])
      (csv/write-csv writer dat-out))))


(defn vec->time-series
  "Converts a vector of values. Returns a seq of maps with two key-value-pairs each,
  one for :week and one for :data. This format is useful for plotting with vega-lite."
  [v]
  (let [steps (range (count v))
        steps-and-vals (zipmap steps v)]
    (for [[t v] steps-and-vals]
      {:week t :data v})))


(defn vecs->time-series
  "Converts a nested vector or seq of values. Wrapper around vec->time-series."
  [vec]
  (flatten
    (map
      #(vec->time-series %)
      vec)))


(defn extract-for-vega
  "Extracts data for kw from seasons in an anglican sample and converts to a format for
  plotting with vega-lite. Works only for single values, not e.g. for seasons."
  [samples kw]
  (->
    (from-seasons samples kw)
    (vecs->time-series)))


(defn weekly-plot-spec
  "Return a spec for vega-lite to plot data for kw in samples by week."
  [samples kw]
  {:data     {:values (extract-for-vega samples kw)}
   :mark     "tick"
   :encoding {:x {:field "week"
                  :type "nominal"
                  }
              :y {:field "data"
                  :type  "quantitative"}}})


(defn filter-by-week
  "Takes anglican samples and returns only those for specified week."
  [samples week]
  (let
    [seasons-by-week (vecs->time-series (from-results samples [:season]))
     week-only (filter #(= week (get % :week)) seasons-by-week)]
    week-only))


(defn week-histo-spec
  "Returns a spec for vega-lite to plot a histogram of new infections for a specified week."
  [samples week]
  (let [new-all (new-infections-in-seasons samples)
        week-only (map #(nth % week) new-all)]
    {:data     {:values week-only}
     :mark     "bar"
     :encoding {:x {:field "data" :type "quantitative"
                    ;:bin {:maxbins 50}
                    }
                :y {:aggregate "count" :type "quantitative"}}}))


(defn weekly-dists-spec
  "Returns a vega-lite spec for vertically plotting the weekly dists for kw."
  [samples kw]
  {:data     {:values (extract-for-vega samples kw)}
   :encoding {:x {:field :data :type "quantitative"}
              :y {:field :week :type "ordinal"}}
   :mark     "tick"
   })


(defn histo-spec
  "Returns a vega-lite spec for plotting a histogram for seq of vals."
  [vals]
  {:data {:values vals}
   :mark "bar"
   :encoding {:x {:field "data"
                  :type "quantitative"}
              :y {:aggregate "count"
                  :type "quantitative"}}})

(declare hdi-plot-spec)

(defn new-infections-plot-spec
  "Create spec to plot weekly new infections using vega-lite. If cred-mass is supplied, a laayer with
  highest density intervals (one for each week) is added to the plot."
  ([samples]
   {:data     {:values (vecs->time-series (new-infections-in-seasons samples))}
    :mark     {:type "tick" :opacity 0.3}
    :encoding {:x {:field :week :type "ordinal"}
               :y {:field :data :type "quantitative"
                   :axis  {:title "new infections"}}}})
  ([samples cred-mass]
   {:layer [(new-infections-plot-spec samples)
            (hdi-plot-spec samples cred-mass)]}))


(defn hdi [samples-from-dist cred-mass]
  "Compute highest density interval for probability distribution represented by samples-from-dist.
  Algorithm is adopted from Kruschke, J.K. (2015)."
  (let [sorted (sort samples-from-dist)
        ;;number of samples-from-dist needed with given cred-mass
        n-keep (int (Math/floor (* cred-mass (count sorted))))
        ;;how many CIs to compare
        n-CIs (- (count sorted) n-keep)
        ;;computing widths for the different CIs
        ci-width (reduce #(conj %1 (- (nth sorted (+ %2 n-keep)) (nth sorted %2))) [] (range n-CIs))
        ;;keep the narrowest width
        min-width (apply min ci-width)
        ;;locate index of min-width
        ind-of-min (.indexOf ci-width min-width)
        ;;get hdi borders
        borders [(nth sorted ind-of-min) (nth sorted (+ ind-of-min n-keep))]
        ]
    borders))


(defn samples->weekly-new
  "Takes samples and returns a seq of vectors holding weekly numbers of
  newly infected individuals"
  [samples]
  (->> samples
       (new-infections-in-seasons)
       (apply map vector)))


(defn samples->hdi-borders
  "Compute highest density interval borders for samples as returned by anglican. Wrapper around hdi."
  [samples cred-mass]
  (let [weekly (samples->weekly-new samples)]
    (map #(hdi % cred-mass) weekly)))


(defn borders->vega-lite
  "Takes a nested seq of vectors of length two, holding the low and high border of the hdi. Returns
  a seq of maps with keys representing week numbers and values representing the borders."
  [borders]
  (let [lo (map first borders)
        hi (map second borders)]
    (into (vec->time-series lo) (vec->time-series hi))))


(defn hdi-plot-spec
  [samples cred-mass]
  (let [borders (samples->hdi-borders samples cred-mass)]
    {:data     {:values (borders->vega-lite borders)}
     :mark     {:type "bar" :opacity 0.3 :color "green"}
     :encoding {:x {:field :week :type "ordinal"}
                :y {:field :data :type "quantitative"}}}))


(defn data-plot-spec
  [data]
  {:data     {:values (vec->time-series data)}
   :mark     {:type "tick" :color "red"}
   :encoding {:x {:field :week :type "ordinal"}
              :y {:field :data :type "quantitative"}}})


(defn dashboard-spec
  "Plot a dashboard-like view of different aspects of the simulation data."
  ([samples]
   (let
     [primary (weekly-plot-spec samples :primary)
      secondary (weekly-plot-spec samples :secondary)
      new (new-infections-plot-spec samples 0.95)
      lambda-1 (histo-spec (from-results samples :lambda-1))
      lambda-2 (histo-spec (from-results samples :lambda-2))
      weekly-dists {:data     {:values (vecs->time-series (new-infections-in-seasons samples))}
                    :mark     "tick"
                    :encoding {:x {:field :data :type "quantitative"}
                               :y {:field :week :type "ordinal"}}}

      board {:hconcat
             [{:vconcat [primary secondary new {:hconcat [lambda-1 lambda-2]}]}
              weekly-dists]}
      ]
     board)
    )
  ([samples data]
   (let
     [primary (weekly-plot-spec samples :primary)
      secondary (weekly-plot-spec samples :secondary)
      new-and-data {:layer [(new-infections-plot-spec samples 0.95) (data-plot-spec data)]}
      lambda-1 (histo-spec (from-results samples :lambda-1))
      lambda-2 (histo-spec (from-results samples :lambda-2))
      weekly-dists {:data     {:values (vecs->time-series (new-infections-in-seasons samples))}
                    :mark     "tick"
                    :encoding {:x {:field :data :type "quantitative"}
                               :y {:field :week :type "ordinal"}}}

      board {:hconcat
             [{:vconcat [primary secondary new-and-data {:hconcat [lambda-1 lambda-2]}]}
              weekly-dists]}
      ]
     board)

    ))

