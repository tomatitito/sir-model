(ns clojure-code.functions
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv])
  )

(defn write-to-file [ results param path]
  (with-open [writer (io/writer path)]
    (doseq [line results]
      (.write writer (str (param (first (:predicts line))) "\n"))
      )
    )
  )
