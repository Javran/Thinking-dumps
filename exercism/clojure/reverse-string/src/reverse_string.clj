(ns reverse-string)

(defn reverse-string [n]
  (apply str (into () n)))
