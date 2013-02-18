(ns clojure-codes.day-2.try.unless
  (:require 
    [clojure-codes.utils :as utils]))

; this version of unless will not work
;     since 'body' will always be evaluated
(defn unless-bad [test body] (if (not test) body))

(defmacro unless [test body]
      (list 'if (list 'not test) body))

(defn -main [& args]
  (unless-bad true (println "Danger, danger Will Robinson"))

  (utils/eval-and-println

    (macroexpand ''something-we-do-not-want-interpreted)
    (macroexpand '#(count %))
  )

  (utils/eval-and-println
    (macroexpand '(unless condition body))
  )
  (unless true (println "No more danger, Will."))
  (unless false (println "Now, THIS is The FORCE."))

)
