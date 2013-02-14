(ns clojure-codes.day-1.try.bool
  (:require [clojure-codes.utils :as utils]))

(defn -main [& args]
  (utils/eval-and-println 
    ; use == to do numeric comparison in clojure 1.4
    (== 1 1.0)
    ; true
    (= 1 1.0)
    ; false
    (== 1 2)
    ; false
    (< 1 2)
    ; true

    ; clojure uses some java classes
    (class true)
    (class false)
    (class (== 1 1.0))
    ; java.lang.Boolean

    ; if statement
    (if true "True it is.")
    (if (> 1 2) "True it is.")
    ; line above prints nothing
    (if (< 1 2)
      "False it is not.")
    ; if-else statement
    (if false
      "true"
      "false")

    (first ())
    ; nil

    ; 0 / "" -> true
    ; nil -> false
    (if 0
      "true"
      "false")
    ; true
    (if nil
      "true"
      "false")
    ; false
    (if ""
      "true"
      "false")
    ; true
  )
)
