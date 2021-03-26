(ns run-length-encoding)

(set! *warn-on-reflection* true)
(def char-numeric? #(Character/isDigit ^char %))
(def str->int #(Integer/parseInt ^chars %))

(defn encode-chunk
  "encodes a non-empty sequence of same char"
  [xs]
  (let [[hd & tl] xs
        cnt (count xs)]
    (if (empty? tl)
      (str hd)
      (str cnt hd))))

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
   cipher-text
   (re-seq #"(\d+)(.)|\D+")
   (map
    (fn [[orig num-str ch]]
      (if ch
        (apply str (repeat (str->int num-str) ch))
        orig)))
   (apply str)))
