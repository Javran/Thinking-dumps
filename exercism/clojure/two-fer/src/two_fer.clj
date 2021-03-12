(ns two-fer)

(defn two-fer [& may-name]
  (if (nil? may-name)
    "One for you, one for me."
    (str "One for " (first may-name) ", one for me.")))
