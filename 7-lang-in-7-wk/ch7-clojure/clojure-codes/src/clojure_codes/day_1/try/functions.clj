(ns clojure-codes.day-1.try.functions
  (:require 
    [clojure-codes.utils :as utils]))

(defn -main [& args]
  (utils/eval-and-println
    (defn force-it-1 
      "The first function a young Jedi needs"
      []
      (str "Use the force, " "Luke."))
    (force-it-1)
    ; use (doc xxx) in repl to see the document of a given function
    ; e.g. 
    ;     (doc force-it-1)
    ;     (doc str)
    ;     (doc doc)

    (defn force-it-2
      "The first function a young Jedi needs"
      [jedi]
      (str "Use the force, " jedi "."))
    (force-it-2 "Luke")
  )
)
