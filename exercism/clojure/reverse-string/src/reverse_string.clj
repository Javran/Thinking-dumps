(ns reverse-string)

(defn reverse-string [s]
  (apply
   str
   (;; reverse by constructing diff-list
    (reduce (fn [acc i] (comp acc #(conj % i))) identity s) [])))
