(ns run-length-encoding
  (:require [clojure.string :as str]))

(defn encode-chunk
  "encodes a non-empty sequence of same char"
  [xs]
  (let [[hd & tl] xs
        cnt (count xs)]
    (if (empty? tl)
      (str hd)
      (format "%d%s" cnt hd))))

(defn run-length-encode
  "encodes a string with run-length-encoding"
  [plain-text]
  (loop [result-rev nil
         xs plain-text]
    (if (empty? xs)
      (str/join (reverse result-rev))
      (let [[hd & _] xs
            [ys zs] (split-with #(= hd %) xs)]
        (recur (cons (encode-chunk ys) result-rev) zs)))))

(defn decode-chunk
  "decodes a encoded char seq,
  returns `(cons <result> <rest of input>)`
  or nil if input is empty."
  [xs]
  (if (empty? xs)
    nil
    (let [[digits ys] (split-with #(Character/isDigit %) xs)]
      (if (empty? digits)
        (cons (str (first xs)) (rest xs))
        (let [cnt (Integer/parseInt (apply str digits))
              [hd & tl] ys]
          (cons (apply str (repeat cnt hd))
                tl))))))

(defn run-length-decode
  "decodes a run-length-encoded string"
  [cipher-text]
  (loop [result-rev nil
         xs cipher-text]
    (let [result (decode-chunk xs)]
      (if result
        (let [[r & rest] result]
          (recur (cons r result-rev) rest))
        (str/join (reverse result-rev))))))
