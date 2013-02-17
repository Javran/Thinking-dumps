(ns clojure-codes.day-2.try.sequence
  (:require 
    [clojure-codes.utils :as utils]))

(defn -main [& args]
  (utils/eval-and-println
    (every? number? [1 2 3 :four])
    ; false
    (every? number? [1 2 3 4])
    ; true

    (some nil? [1 2 nil])
    ; true
    (some nil? [1 2 3])
    ; nil (please refer to (doc some) for detail)

    (some number? [1 2 :three])
    ; true
    
    (not-every? odd? [1 3 5])
    ; false

    (not-any? number? [:one :two :three])
    ; true

    (def words ["luke" "chewie" "han" "lando"])

    (filter (fn [word] (> (count word) 4)) words)
    ; ["chewie" "lando"]
    (filter #(> (count %) 4) words)
    ; ["chewie" "lando"]

    (map (fn [x] (* x x)) [1 1 2 3 5])
    ; [1 1 4 9 25]
    (map #(* % %) [1 1 2 3 5])
    ; [1 1 4 9 25]

    (def colors ["red" "blue"])
    (def toys ["block" "car"])

    (for [x colors] (str "I like " x))
    (for [x colors, y toys]
      (str "I like " x " " y "s")) 

    (defn small-word? [w] (< (count w) 4))
    (for [x colors, y toys, :when (small-word? y)]
      (str "I like " x " " y "s")) 

    (reduce + [1 2 3 4])
    ; 10
    (reduce * [1 2 3 4 5])
    ; 120
    (reduce 
      (fn [acc i] (str acc "-" i))
      "1"
      [2 3 4 5])

    (sort [3 1 2 4])
    ; 1 2 3 4

    (defn abs [x] (if (< x 0) (- x) x))

    (sort-by abs [-1 -4 3 2])
    ; -1 2 3 -4
  )
)
