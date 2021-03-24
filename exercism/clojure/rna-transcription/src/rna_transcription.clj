(ns rna-transcription)

(defn to-rna [dna]
  (clojure.string/escape
   dna
   (fn [ch]
     (case ch
       \C \G
       \G \C
       \T \A
       \A \U
       (assert false :invalid-char)))))
