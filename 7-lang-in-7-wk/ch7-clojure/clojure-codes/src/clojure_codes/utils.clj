(ns clojure-codes.utils)

(defn eval-and-println
  "eval codes line-by-line, print all results"
  [& args]
  (doseq [x args] (println x)))
