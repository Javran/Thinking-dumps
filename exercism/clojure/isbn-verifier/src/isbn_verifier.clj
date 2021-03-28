(ns isbn-verifier)

(defn digit->int
  [ch]
  (let [r (Character/digit ch 10)]
    (and (not= r -1) r)))

(defn parse-reversed-isbn
  [ds]
  (and
   (seq ds)
   (let [[hd & tl] ds
         hd-r (if (= hd \X)
                ;; special case for last digit
                10
                (digit->int hd))
         tl-r (map digit->int tl)
         r (cons hd-r tl-r)]
     (and (= (count r) 10)
          (every? identity r)
          r))))

(defn isbn? [isbn]
  (let [parsed
        (->>
         isbn
         (into ())
         (filter #(not= % \-))
         parse-reversed-isbn)]
    (and
     parsed
     (let [sum (apply + (map * parsed (range 1 11)))]
       (zero? (rem sum 11))))))
