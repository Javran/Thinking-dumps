(ns clojure-codes.day-2.try.delayed-calculation
  (:require 
    [clojure-codes.utils :as utils]))

(defn -main [& args]
  (utils/eval-and-println
    (range 1 10)
    ; (1 .. 9) exclusive range
    (range 1 10 3)
    ; (1 4 7) step=3
    (range 10)
    ; (0 .. 9) default init num

    ; infinite seq
    (take 3 (repeat "Use the force, Luke"))
    ; take an infinite seq and take first 3 elements

    (def vec-1 [:lather :rinse :repeat])

    ; expand a seq to infinite seq
    (take 5 (cycle vec-1))
    ; (:la :ri :re :la :ri)

    ; can drop heading elements as well
    (take 5 (drop 2 (cycle vec-1)))
    ; (:re :la :ri :re :la)

    ; new operator: `->>`
    (->> vec-1 (cycle) (drop 2) (take 5))
    ; same as the previous call, somewhat like a pipe
    
    ; insert separator between elements
    (take 5 (interpose :and (cycle vec-1)))
    ; [:la :a :ri :a :re]
    (->> vec-1 (cycle) (interpose :and) (take 5))
    ; [:la :a :ri :a :re]

    (take 20 (interleave (cycle (range 2)) (cycle (range 3))))
    (->> (range 3) cycle (interleave (cycle (range 2))) (take 20))
    ; I think this example is obscure, let's try to use chars 
    (take 20 (interleave (cycle [\a \b \c]) (cycle [:d :e])))
    ; easy to see that the elements are taken alternatively
    ; `interleave` example for 3 collections
    (take 20 (interleave (cycle [\a \b \c]) (cycle [:d :e]) (cycle [1 2])))
    ; please refer to `(doc interleave)` if you want to know more about this function
    
    ; use iteration to create a seq
    (take 5 (iterate inc 1))
    ; (1 2 3 4 5)
    (take 5 (iterate dec 0))
    ; (0 -1 -2 -3 -4)
    
    ; make a list for 3n+1 problem
    (take 20 (iterate 
              #(if (odd? %) 
                (+ (* 3 %) 1)
                (/ % 2))
              11))
    ; we can see the seq eventually run into a cycle: 4 2 1 4 2 1 ...

    (defn fib-pair
      [[a b]] [b (+ a b)])

    (fib-pair [3 5])
    ; use bigint here or you might encounter an integer overflow exception
    ;     when calculating the 500-th one in fibonacci seq
    (def fib-seq (map first (iterate fib-pair [(bigint 1) (bigint 1)])))
    (->> fib-seq (take 5))
    ; fibonacci seq
    (nth fib-seq 500)

    ; factorial
    (defn factorial [n]
      (apply * (take n (iterate inc 1))))
    (factorial 5)
    ; 120
    
  )
)
