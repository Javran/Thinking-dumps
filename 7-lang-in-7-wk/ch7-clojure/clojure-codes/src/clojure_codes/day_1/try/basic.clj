(ns clojure-codes.day-1.try.basic)

(defn -main [& args]
  (println "nice boat!")
  (println "Give me some Clojure!")
  (println (- 1))
  ; this is 1+1
  (println (+ 1 1))
  ; and 10*10
  (println (* 10 10))

  ; Ratio
  (println (/ 1 3))
  (println (/ 2 4))
  ; diff from others, this one outputs "0.5"
  (println (/ 2.0 4))
  
  (println (class (/ 1 3)))

  ; 5 mod 4
  (println (mod 5 4))
  ; 1

  ; more examples!
  (println (/ (/ 12 2) (/ 6 2)))
  ; 2

  (println (+ 2 2 2 2))
  ; 8
  (println (- 8 1 2))
  ; 5
  (println (/ 8 2 2))
  ; 2

  ; judge if a seq is sorted
  (println (< 1 2 3))
  ; true
  (println (< 1 3 2 4))
  ; false

  ; numeric comparison
  (println (== 1 1.0 1e0))
  ; true

  ; more like 'equals' in Java
  ;     types are taken into account
  (println (= 1 1.0 1e0))
  ; false

  ; try type conversion
  (println (+ 3.0 5))
  (println (class (+ 3.0 5)))

  (println (+ 3 5.0))
  (println (class (+ 3 5.0)))

  )
