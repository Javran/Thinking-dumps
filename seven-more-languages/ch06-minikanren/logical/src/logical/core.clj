(ns logical.core
  (:gen-class))

(use 'logical.day1-easy)
(use 'logical.day1-medium)
(use 'logical.day1-hard)

(defn -main
  [& args]
  (day1-easy)
  (day1-medium)
  (day1-hard))
