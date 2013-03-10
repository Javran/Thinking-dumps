(ns clojure-codes.day-3.try.whatelse
  (:require 
    [clojure-codes.utils :as utils]))

(defn -main [& args]
  (utils/eval-and-println
    ; you can find how to use 'with-meta' at:
    ;     http://clojuredocs.org/clojure_core/clojure.core/with-meta
    (def data-with-meta (with-meta [1 2 3] {:meta :meta-this-is}))

    ; access meta data
    ((meta data-with-meta) :meta)

    ; compatible with Java
    (.toUpperCase "Fred")

    (str (.reverse (StringBuilder. "!taob eciN")))
    ; Nice boat!
  )
)
