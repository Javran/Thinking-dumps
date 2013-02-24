(ns clojure-codes.day-2.try.loop-and-recur
  (:require 
    [clojure-codes.utils :as utils]))

(defn size-1 [v]
  (if (empty? v)
    0
    (inc (size-1 (rest v)))))

(defn size-2 [v]
  (loop [l v, c 0]
    (if (empty? l)
      c
      (recur (rest l) (inc c)))))

(defn -main [& args]
  (utils/eval-and-println
    (def vectors [
      []
      [1 2]
      [1 2 3 4]])
    (map size-1 vectors)
    (map size-2 vectors)
    
    ; `loop` can be regarded as `let`
    (loop [x 1] x)
    ; 1
  )
)
