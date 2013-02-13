(ns clojure-codes.day-1.try.bool)

(defn -main [& args]
  ; use == to do numeric comparison in clojure 1.4
  (println (== 1 1.0))
  ; true
  (println (= 1 1.0))
  ; false
  (println (== 1 2))
  ; false
  (println (< 1 2))
  ; true

  ; clojure uses some java classes
  (println (class true))
  (println (class false))
  (println (class (== 1 1.0)))
  ; java.lang.Boolean

  ; if statement
  (if true (println "True it is."))
  (if (> 1 2) (println "True it is."))
  ; line above prints nothing
  (if (< 1 2)
    (println "False it is not."))
  ; if-else statement
  (if false
    (println "true")
    (println "false"))

  (println (first ()))
  ; nil

  ; 0 / "" -> true
  ; nil -> false
  (if 0
    (println "true")

    (println "false"))
  ; true
  (if nil
    (println "true")
    (println "false"))
  ; false
  (if ""
    (println "true")
    (println "false"))
  ; true
  )
