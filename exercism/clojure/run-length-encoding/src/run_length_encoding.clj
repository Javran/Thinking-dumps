(ns run-length-encoding)

(defn encode-chunk
  "encodes a non-empty sequence of same char"
  [xs]
  (let [[hd & tl] xs
        cnt (count xs)]
    (if (empty? tl)
      (str hd)
      (str cnt hd))))

(def char-numeric? #(Character/isDigit %))

(defn run-length-encode
  "encodes a string with run-length-encoding"
  [plain-text]
  (->>
   (partition-by identity plain-text)
   (map encode-chunk)
   (apply str)))

(defn run-length-decode
  "decodes a run-length-encoded string"
  [cipher-text]
  (->>
   (partition-by char-numeric? cipher-text)
   (map
    (fn
      [xs]
      "constructs a continuation that takes the result of decoding rest of the str
       (which is a list of `char?`) and put what `xs` decodes to in front of it"
      (if (char-numeric? (first xs))
        (let [cnt (Integer/parseInt (apply str xs))]
          (fn [[hd & tl]]
            (concat (repeat cnt hd) tl)))
        #(concat xs %))))
   (reduce comp identity)
   (#(% ""))
   (apply str)))
