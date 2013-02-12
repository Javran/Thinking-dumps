(ns clojure-codes.day-1.try.str-and-char)

(defn -main [& args]
  ; convert to string
  (println (str 1))
  (println (class (str 1)))

  ; concat strings together
  (println (str "yoda, " "luke, " "darth"))
  ; even non-strings
  (println (str "one: " 1 " two: " 2))

  ; char should have a leading '\'
  (println \a \b \c \newline)

  (println (str \f \o \r \c \e))

  ; some comparison
  (println (= "a" \a))
  ; false

  (println (= (str \newline) "\n"))
  ; true
  )
