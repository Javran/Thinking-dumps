(ns clojure-codes.day-2.do.do
  (:require 
    [clojure-codes.utils :as utils]))

(defmacro unless 
  "Task #1: implement a unless macro with else-condition"
  [test true_body false_body]
  (list 'if (list 'not test) true_body false_body))

(defprotocol Seesaw
  "Task #2: write a type to implement a protocol using defrecord"
  ; let's make a seesaw:
  ; every object has a weight
  ; every object can be divided into smaller pieces
  ; seesaw has two sides: `:l` and `:r`
  ; seesaw has 3 statuses: `:leftdown` `:rightdown` and `:balance`

  ; all functions below will return a tuple: (a,s)
  ;     where a is the return value and s is new status
  ;     so that we can make our Seesaw iterable
  ; in other word, all interfaces below satisfies type:
  ;     s -> (a, s)
  ; just like State monad in haskell

  (add [this where weight])
  (rm [this where weight])
  (status [this])
  )

(defrecord SeesawImpl [l_weight r_weight]
  Seesaw

  (add [_ where weight]
    (defn info_str
      [where diff]
      (str "Update weight(diff=" diff ", side=" where ")"))
    (if (= where :l)
      [
        (info_str where weight)
        (SeesawImpl. (+ l_weight weight) r_weight)]
     (if (= where :r)
       [
        (info_str where weight)
        (SeesawImpl. l_weight (+ r_weight weight))]
       :error)))

  (rm [this where weight]
    (add this where (- weight)))

  (status [this]
    (defn info_str
      [s]
      (str "balance status: " s))
    (if (> l_weight r_weight)
      [(info_str :leftdown) this]
      (if (< l_weight r_weight)
        [(info_str :rightdown) this]
        [(info_str :balance) this])))

  Object
    (toString [_]
      (str "SeesawImpl(l=" l_weight ", r=" r_weight ")"))
) 

(defn -main [& args]
  (utils/eval-and-println
    (unless true "False This is" "True this is")
  )

  (reduce
    (fn [curr_stat operation]
      (println (str "Status: " curr_stat))
      (def new_v (operation curr_stat))
      (println (str "Operation Result: " (first new_v)))
      (println (str "New status: " (second new_v)))
      (println)
      (second new_v))
   (SeesawImpl. 0 0)
   [
    ; add 10 to left side
    #(add % :l 10)
    ; we can see left side moving down
    status
    ; add 20 to right side
    #(add % :r 20)
    ; now right side is moving down
    status
    ; remove 14 from right side
    #(rm % :r 14)
    ; remove 4 from left side
    #(rm % :l 4)
    ; we can see the seesaw get balanced again
    status
   ]) 
)
