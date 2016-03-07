(ns logical.core
  (:gen-class))

(use 'logical.day1-easy)
(use 'logical.day1-medium)
(use 'logical.day1-hard)
(use 'logical.day2-easy)
(use 'logical.day2-medium)
(use 'logical.day2-hard)
(use 'logical.day3-easy)
(use 'logical.day3-medium)
(use 'logical.day3-hard)

(defn -main
  [& args]
  (day1-easy)
  (day1-medium)
  (day1-hard)

  (day2-easy)
  (day2-medium)
  (day2-hard)

  (day3-easy)
  (day3-medium)
  (day3-hard))
