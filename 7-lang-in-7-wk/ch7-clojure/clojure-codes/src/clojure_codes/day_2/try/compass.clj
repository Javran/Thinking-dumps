(ns clojure-codes.day-2.try.compass
  (:require 
    [clojure-codes.utils :as utils]))

(defprotocol Compass
  (direction [c])
  (left [c])
  (right [c]))

(def directions [:north :east :south :west])
; :north  0
; :east   1
; :south  2
; :west   3

(defn turn
  [base amount]
  (rem (+ base amount) (count directions)))

(defrecord SimpleCompass [bearing]
  Compass
  (direction [_] (directions bearing))
  (left [_] (SimpleCompass. (turn bearing 3)))
  (right [_] (SimpleCompass. (turn bearing 1)))
  
  Object 
  (toString [this] (str "[" (direction this) "]")))

(defn -main [& args]
  (utils/eval-and-println
    (turn 1 1)
    ; :east + left -> :south
    (turn 3 1)
    (turn 2 3)

    (def c (SimpleCompass. 0))
    c
    ; compass: bearing 0
    (left c)
    ; compass: bearing 3
    c
    ; compass: bearing 0
    (right c)
    ; compass: bearing 1

    (:bearing (left (left c)))
    ; 2
  )
)
