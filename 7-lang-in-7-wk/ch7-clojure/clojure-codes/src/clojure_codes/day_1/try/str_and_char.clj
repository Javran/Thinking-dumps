(ns clojure-codes.day-1.try.str-and-char
  (:require [clojure-codes.utils :as utils]))

(defn -main [& args]
  (utils/eval-and-println
    ; convert to string
    (str 1)
    (class (str 1))

    ; concat strings together
    (str "yoda, " "luke, " "darth")
    ; even non-strings
    (str "one: " 1 " two: " 2)

    ; char should have a leading '\'
    '(\a \b \c \newline)

    (str \f \o \r \c \e)

    ; some comparison
    (= "a" \a)
    ; false

    (= (str \newline) "\n")
    ; true
    )

  )
