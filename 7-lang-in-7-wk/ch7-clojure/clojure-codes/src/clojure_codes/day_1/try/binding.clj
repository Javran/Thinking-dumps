(ns clojure-codes.day-1.try.binding
  (:require 
    [clojure-codes.utils :as utils]))

(defn -main [& args]
  (utils/eval-and-println
    (def line [[0 0] [10 20]])
    line

    (defn line-end-1 
      "get the end of a line"
      [ln] (last ln))
    (line-end-1 line)
    
    (defn line-end-2
      "get the end of a line"
      [[_ second]]
      second)
    (line-end-2 line)

    (def board [
      [:x :o :x]
      [:o :x :o]
      [:o :x :o]])

    (defn center-1 [ [_ [_ c _] _] ] c)
    (center-1 board)

    ; omitting some underlines
    (defn center-2 [ [_ [_ c] ] ] c)
    (center-2 board)

    ; use `let`
    (defn center-3
      [board]
      ; just like unification in prolog saying:
      ; center(Board,C) :- 
      ;     [_, [_,C | _]| _] = Board.
      (let [[_ [_ c]] board] c))
    (center-3 board)
    
    ; use `nth`
    (defn center-4
      [board]
      (nth (nth board 1) 1))
    (center-4 board)

    ; destructing map
    (def person {:name "Jabba", :profession "Gangster"})
    (let [{name :name} person]
      (str "The person's name is " name))

    (def villains
      [{:name "Godzilla" :size "big"}
       {:name "Ebola" :size "small"}])

    (let [[_ {name :name}] villains]
      (str "Name of the second villains: " name))

  )
)
