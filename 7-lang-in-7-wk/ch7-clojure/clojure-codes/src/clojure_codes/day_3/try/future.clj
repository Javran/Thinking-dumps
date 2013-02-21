(ns clojure-codes.day-3.try.future
  (:require 
    [clojure-codes.utils :as utils]))

(defn -main [& args]
  (utils/eval-and-println
    (def finer-things (future (Thread/sleep 1000) "take time"))
    ; will block here until the calculation is done
    @finer-things

    (def things-1 (future (Thread/sleep 1000) "things-1"))
    (def things-2 (future (Thread/sleep 2000) "things-2"))
    (def things-3 (future (Thread/sleep 3000) "things-3"))
    (def things-4 (future (Thread/sleep 4000) "things-4"))

    ; will sleep 4 sec rather than 10 sec
    ;     since the calculation is done concurrently
    [@things-1 @things-2 @things-3 @things-4]
  )
)
