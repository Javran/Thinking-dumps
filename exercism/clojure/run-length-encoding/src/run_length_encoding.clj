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

(def char-numeric? #(Character/isDigit %))

(defn run-length-encode
  "encodes a string with run-length-encoding"
  [plain-text]
  (apply str (map encode-chunk (partition-by identity plain-text))))

(defn run-length-decode
  "decodes a run-length-encoded string"
  [cipher-text]
  ((reduce
    comp
    identity
    (map
     (fn [xs]
       (if (char-numeric? (first xs))
         (let [cnt (Integer/parseInt (apply str xs))]
           (fn [s]
             (let [[hd & tl] (map identity s)]
               (str (apply str (repeat cnt hd)) (apply str tl)))))
         (fn [ys] (str (apply str xs) ys))))
     (partition-by char-numeric? cipher-text)))
   ""))

(def
  e
  (run-length-decode "aaa2v3d12EFG"))
