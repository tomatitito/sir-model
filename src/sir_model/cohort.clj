(ns sir-model.cohort
  (:use [anglican [core :exclude [-main]] runtime emit]))


(defrecord SIR-Compartments
  [S I R] )


(defn create-compartments-coll
  "Creates a map of length timesteps with each entry being an SIR-Compartments record."
  ([timesteps] (create-compartments-coll timesteps []))
  ([timesteps coll] (into coll (repeatedly timesteps #(->SIR-Compartments 0 0 0)))))


(defn initialize-compartments-coll
  [coll n-susceptible initially-infected]
  (-> coll
      (assoc-in [0 :S] n-susceptible)
      (assoc-in [0 :I] initially-infected)))


(defn create-and-init-compartments-coll
  [timesteps n-susceptible initially-infected]
  (-> timesteps
      (create-compartments-coll)
      (initialize-compartments-coll n-susceptible initially-infected)))


(defm start-cohort
      [t-cur lambda-old lambda-new compartments-coll & data]
      (let [old-n (get-in compartments-coll [t-cur :I])
            new-n (sample (poisson (* old-n lambda-old)))
            secondary (sample (poisson (* new-n lambda-new)))
            weekly-cases (+ new-n secondary)
            still-susceptible (- (get-in (first compartments-coll) [:S]) weekly-cases)]

        ;; if data available
        (when data
          (observe (poisson (+ (* old-n lambda-old) (* new-n lambda-new))) (first data)))

        ;; update compartments-map based on computations
        (-> compartments-coll
            (assoc-in ,,, [t-cur :I] (+ (get-in compartments-coll [t-cur :I]) weekly-cases))
            (assoc-in ,,, [t-cur :S] (- (get-in compartments-coll [t-cur :S]) weekly-cases)))))


(defm progress
  "Progression of a cohort. After new individuals have been infected (which happened in the function start-cohort), at
   each timestep (starting one timestep after the start of the cohort) some people from the cohort recover. Progression
   continues until time is up. Returns the compartments-map."
  [t-cur recovery-param compartments-map]
  (loop [;; recovery starts at the timestep after infection
         ;; this makes sure that all newly infected can generate
         ;; new cases at their time of infection
         t (inc t-cur)
         cases (get-in compartments-map [t-cur :I])
         compartments compartments-map]

    (if (= t (count compartments-map))
      compartments

      (let
        [;; IMPORTANT: number of susceptibles has to remain
         ;; constant during progression
         susceptible (get-in compartments-map [t-cur :S])

         removed (sample* (binomial cases recovery-param))
         still-inf (max 0 (- cases removed))

         updated-compartments
         (-> compartments
             (assoc-in ,,, [t :S] susceptible)
             (assoc-in ,,, [t :I] (+ (get-in compartments [t :I]) still-inf))
             (assoc-in ,,, [t :R] (+ (get-in compartments [t :R]) (get-in compartments [(dec t) :R]) removed)))]

        (recur (inc t)
               still-inf
               updated-compartments)))))


(defm cohort-progression
      [t-cur lambda-old lambda-new recovery-param compartments-coll & data]
      (if data
        (progress t-cur recovery-param
                  (start-cohort t-cur lambda-old lambda-new compartments-coll data))
        (progress t-cur recovery-param
                  (start-cohort t-cur lambda-old lambda-new compartments-coll))))

(defm test-cohort
      [t-cur lambda-old lambda-new recovery-param coll]
      (start-cohort t-cur lambda-old lambda-new coll))


(with-primitive-procedures
  [create-and-init-compartments-coll]
  (defquery
    simple-poisson-process-model
    [n-weeks prog-fn n-susceptibles initially-infected & data]
    (let
      [
       shape (sample (gamma 20 5))
       R-0-dist (gamma shape 5)
       R-0 (sample R-0-dist)

       lambda-old (sample (uniform-continuous 0.3 0.4))
       lambda-new (sample (uniform-continuous 0.4 0.6))
       recovery-par 0.5
       initial-coll (create-and-init-compartments-coll n-weeks n-susceptibles initially-infected)


       season-fn (fn season-fn [week n-weeks par-1 par-2 recov coll & data]
                   (if (= week n-weeks)
                     coll

                     (if data
                       (let [[head & tail] data
                             updated-coll (prog-fn week par-1 par-2 recov coll head)]
                         (season-fn (inc week) n-weeks par-1 par-2 recov updated-coll tail))

                       (let [updated-coll (prog-fn week par-1 par-2 recov coll)]
                         (season-fn (inc week) n-weeks par-1 par-2 recov updated-coll)))
                     )
                   )

       season-data (season-fn 0 n-weeks lambda-old lambda-new recovery-par initial-coll)
       ]


      {:season season-data :lambda-old lambda-old :l-new lambda-new})))
