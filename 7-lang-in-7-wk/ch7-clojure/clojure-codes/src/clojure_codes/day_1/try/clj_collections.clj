(ns clojure-codes.day-1.try.clj-collections
  (:require 
    [clojure.set]
    [clojure-codes.utils :as utils]))

(defn -main [& args]
  (utils/eval-and-println

    ; let's make lists
    (list 1 2 3)
    (class (list 1 2 3))

    '(1 2 3)
    (class '(1 2 3))

    (def list1 '(:r2d2 :c3po)) 

    (first list1)
    ; :r2d2

    (last list1)
    ; :c3po
    (rest list1)
    ; (:c3po)
    (cons :battle-droid list1)
    ; (:battle-droid :r2d2 :c3po)

    ; time for vectors
    (def vec1 [:hutt :wookie :ewok])
    vec1
    (first vec1)
    ; :hutt
    (nth vec1 2)
    ; :ewok
    (nth vec1 0)
    ; :hutt
    (last vec1)
    ; :ewok
    (vec1 1)
    ; :wookie

    ; concat vectors
    (concat [:darth-vader] [:darth-maul])
    ; note here prints a list
    ; (:darth-vader :darth-vader)
    (class (concat [:darth-vader] [:darth-maul]))
    ; ok, it's actually a 'LazySeq'
    (first vec1)
    ; :hutt
    (rest vec1)
    ; (:wookie :ewok)

    ; set show time
    (def spacecraft #{:x-wing :y-wing :tie-fighter})
    (class spacecraft)
    (count spacecraft)
    (sort spacecraft)
    
    (sorted-set 2 3 1)

    (clojure.set/union #{:skywalker} #{:vader})
    (clojure.set/difference #{1 2 3} #{2})

    ; set itself is a function that judge if an element is one member of it
    (#{:jar-jar :chewbacca} :chewbacca)
    ; :chewbacca
    (#{:jar-jar :chewbacca} :luke)
    ; :luke

    ; about map
    {:chewie :wookie :lea :human}
    ; that means 
    ; :chewie -> :wookie
    ; :lea -> :human
    
    ; another form
    (def mentors {:darth-vader "obi wan", :luke "yoda"})
    (class mentors)
    ; ArrayMap
    (mentors :luke)
    ; yoda
    ; strange but work
    (:luke mentors)

    ; merge 2 dicts:
    (merge {:y-wing 2, :x-wing 4} {:tie-fighter 2})
    ; another merge that allow conflict values combined to produce one
    (merge-with + {:y-wing 2, :x-wing 4} {:tie-fighter 2, :x-wing 3})
    ; :x-wing -> 7 because 4+3

    ; can also append new k-v pair
    (assoc {:one 1, :three 3} :two 2)

    ; sorted map
    (sorted-map 1 :one, 3 :three, 2 :two)

  )
)
