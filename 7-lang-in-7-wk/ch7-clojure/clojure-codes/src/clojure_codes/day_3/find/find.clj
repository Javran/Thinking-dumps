(ns clojure-codes.day-3.find.find
  (:require 
    [clojure-codes.utils :as utils]
    [clojure.contrib.seq-utils :as seq-utils]))

; use command `lein run -m clojure-codes.day-3.find.find` to see results
;     because `:Eval` command in vim might not show outputs instantly

(defn -main [& args]
  (defn filler
    [fill]
    (loop [x 0]
      ; offer x, sleep for x seconds, x <- [0 .. ]
      (fill x)
      (Thread/sleep (* x 1000))
      (recur (inc x))))

  (def fq (seq-utils/fill-queue filler))

  ; we only wait for 5 elements to come
  (doseq [x (take 5 fq)] 
    (do
      (println "Waiting for elements ...")                                         
      (println x)
      (flush)))

  (System/exit 0)
)
