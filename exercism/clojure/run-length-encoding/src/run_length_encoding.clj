(ns run-length-encoding
  (:require [clojure.string :as str]))

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
  (apply str (map encode-chunk (partition-by identity plain-text))))

(defn decode-chunk
  "decodes a encoded char seq,
  returns `[<result> <rest of input>]`
  or nil if input is empty."
  [xs]
  (if (empty? xs)
    nil
    (let [[digits ys] (split-with #(Character/isDigit %) xs)]
      (if (empty? digits)
        [(str (first xs)) (rest xs)]
        (let [cnt (Integer/parseInt (apply str digits))
              [hd & tl] ys]
          [(apply str (repeat cnt hd)) tl])))))

(defn unfoldr
  [step state]
  (let [result (step state)]
    (if result
      (cons (first result) (unfoldr step (second result)))
      nil)))

(defn run-length-decode
  "decodes a run-length-encoded string"
  [cipher-text]
  (apply str
         (unfoldr decode-chunk cipher-text)))
