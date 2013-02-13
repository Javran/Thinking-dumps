(ns clojure-codes.utils)

(defn eval-and-println [& args] 
  "eval codes line-by-line, print all results"
  (doseq [x args] (println x)))
