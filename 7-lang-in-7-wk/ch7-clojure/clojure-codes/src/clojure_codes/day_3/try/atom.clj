(ns clojure-codes.day-3.try.atom
  (:require 
    [clojure-codes.utils :as utils]))

(defn -main [& args]
  (utils/eval-and-println
    (def danger (atom "Split at your own risk."))
    danger
    @danger

    (reset! danger "Split with impunity")
    @danger

    (def top-sellers (atom []))
    ; conj will insert element into a collection
    (swap! top-sellers conj {:title "Seven Languages in Seven Weeks", :author "Tate"})
    (swap! top-sellers conj {:title "Programming Clojure", :author "Halloway"})
    @top-sellers
  )
)
