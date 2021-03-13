(ns reverse-string)

(defn reverse-string [s]
  (comp
   (partial apply str)
   #(% [])
   ;; reverse by constructing diff-list
   (partial reduce (fn [acc i] (comp acc #(conj % i))) identity)))
