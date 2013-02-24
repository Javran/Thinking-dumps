(ns clojure-codes.day-3.try.ref-deref
  (:require 
    [clojure-codes.utils :as utils]))

(defn -main [& args]
  (utils/eval-and-println
    (ref "Attach of the Clones")
    (def movie (ref "Star Wars"))
    movie
    ; two ways to de-ref: deref or leading '@'
    (deref movie)
    ; Star Wars
    @movie
    ; Star Wars

    ; the line below will cause error
    ;     because "No transaction running"
    ; (alter movie str ": The Empire Strikes Back")

    (dosync (alter movie str ": The Empire Strikes Back"))
    ; found movie pointing to the new value
    @movie

    ; change what it points to totally
    (dosync (ref-set movie "Star Wars: The Revenge of the Sith"))
    @movie

  )
)
