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
  (apply str
         ((reduce
           comp
           identity
           (map
            (fn [xs]
              (if (char-numeric? (first xs))
                (let [cnt (Integer/parseInt (apply str xs))]
                  (fn [[hd & tl]]
                    (concat (repeat cnt hd) tl)))
                #(concat xs %)))
            (partition-by char-numeric? cipher-text)))
          "")))

(def
  e
  (run-length-decode "aaa2v3d12EFG"))
