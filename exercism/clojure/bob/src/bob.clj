(ns bob
  (:require [clojure.string :as str]))

(def letter?
  #(Character/isLetter %))
(def lower?
  #(Character/isLowerCase %))

(defn response-for [s]
  (let [msg (str/trim s)
        yelling? (and (some letter? msg)
                      (not (some lower? msg)))
        question? (str/ends-with? msg "?")]
    (cond
      (empty? msg)  "Fine. Be that way!"
      (and yelling? question?) "Calm down, I know what I'm doing!"
      yelling? "Whoa, chill out!"
      question? "Sure."
      :else "Whatever.")))
