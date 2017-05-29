(ns clojure-code.functions
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
