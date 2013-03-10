(ns clojure-codes.day-3.try.atomcache
  (:require 
    [clojure-codes.utils :as utils]))

(defn accreate
  []
  (atom {}))

(defn acget
  [cache key]
  (@cache key))

(defn acput
  ([cache value-map]
    (swap! cache merge value-map))
  ([cache key value]
    (swap! cache assoc key value)))

(defn -main [& args]
  (utils/eval-and-println
    (def ac (accreate))
    (acput ac :quote "I'm your father, Luke.")
    (str "Cached item: " (acget ac :quote))
  )
)
