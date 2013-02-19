(ns clojure-codes.day-2.find.find
  (:require 
    [clojure-codes.utils :as utils]))

; the example here is excerpted from `clojure-codes.day-2.try.delayed-calculation`
; 3n+1 problem
(defn problem_3n_plus_1
  [num]
  (iterate 
    #(if (odd? %) 
       (+ (* 3 %) 1) ; 3n+1 if n is odd
       (/ % 2)       ; n/2 if n is even
    ) num))


(defn -main [& args]
  (utils/eval-and-println
    (map 
      #(take 20 (problem_3n_plus_1 %))
      (range 9 15))
  )
)
