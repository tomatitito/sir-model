(ns util.functions
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]))


(defn write-to-file [ results param path]
  (with-open [writer (io/writer path)]
    (doseq [line results]
      (.write writer (str (param (first (:predicts line))) "\n")))))


(defn extract-result
  "Extracts data for given keyword from results and outputs a vector."
   [query-output keyw]
   (for [x query-output] (keyw (:result x))))


(defn format4barchart
  "Takes data as produced by frequencies-function and brings them in
   in the right format for proto-repl barchart."
  [freqs]
  (for [x freqs] (second x)))


(defn from-frequencies [freqs position]
  (for [x freqs] (nth x position)))


(defn print-infections-in-cohort [the-map]
  "Prints only the values of cur-inf-map"
  (dotimes [i (count the-map)]
    (print (deref ((keyword (str i)) the-map)) " "))
  (println))


(defn ref-type-map [ref-type size]
  "Create map where each entry is of specified reference type."
  (zipmap
    (map #(keyword (str %)) (range size))
    (repeatedly size #(ref-type 0))) )
