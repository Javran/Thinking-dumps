(ns clojure-codes.day-1.do.do
  (:require 
    [clojure-codes.utils :as utils]))

(defn big 
  "Task #1: write (big st n), which returns true if the length of st does not exceed n"
  [st n]
  (<= (count st) n))


; don't rely on the concrete types of Clojure data structure 
;     or you might encounter some strange problem
; please refer to:
;     http://stackoverflow.com/questions/14909312/in-clojure-why-the-type-of-an-empty-list-is-different-from-that-of-non-empty-li
; for more detail
; it seems using `list?` `map?` `vector?` instead is a good practice
(defn collection-type
  "Task #2: return the type of a given collection(:list, :map, :vector)"
  [col]
  (def type_symbol 
    [
      [list? :list]
      [map? :map]
      [vector? :vector]])
  (reduce
    (fn 
      [acc i] 
      (if (= acc :unknown)
        (if ((first i) col)
          (second i)
          :unknown)
        acc))
    :unknown
    type_symbol))

(defn -main [& args]
  (utils/eval-and-println
    (map #(big % 5) 
         ["", "abcd" "abcdef", "abcde"])
    ; expected output: (true, true, false, true)

    (map collection-type
        [
          []
          [1 2 3]
          ()
          (list 1 2 3)
          '(1 2 3)
          {}
          {1 :one, 2 :two, 3 :three}
        ])
    ; expected output: (:v :v :l :l :l :m :m)
  )
)
