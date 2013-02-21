(ns clojure-codes.day-3.try.agent
  (:require 
    [clojure-codes.utils :as utils]))

(defn -main [& args]
  (utils/eval-and-println
    (defn twice [x] (* 2 x))
    
    ; create an agent with internal state '1'
    (def tribbles (agent 1))

    ; agent will run (twice 1) after receiving this message
    (send tribbles twice)
    @tribbles
    ; 1 or 2 (unstable)
    ; wait a minute
    (Thread/sleep 10)
    
    ; now the internal state of tribbles has been changed
    @tribbles
    ; 2

    ; assume that slow-twice will spent lots of time to run
    (defn slow-twice [x]
      (do
        (Thread/sleep 100)
        (* 2 x)))

    (send tribbles slow-twice)
    @tribbles
    ;2
    (Thread/sleep 200)
    @tribbles
    ; 4

    ; or we can wait for the result
    (send tribbles slow-twice)
    (await tribbles)
    @tribbles
    ; 8
  )
)
