(ns clojure-codes.day-2.do.do
  (:require 
    [clojure-codes.utils :as utils]))

(defmacro unless 
  "Task #1: implement a unless macro with else-condition"
  [test true_body false_body]
  (list 'if (list 'not test) true_body false_body))

(defn -main [& args]
  (utils/eval-and-println
    (unless true "False This is" "True this is")
  )
)
