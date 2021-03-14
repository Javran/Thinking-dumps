(ns bob
  (:require [clojure.string :as str]))

(def letter?
  #(Character/isLetter %))
(def lower?
  #(Character/isLowerCase %))

(defn response-for [s]
  (let [msg (str/trim s)]
    (if (empty? msg)
      "Fine. Be that way!"
      (let [all-cap
            (and (some letter? msg)
                 (not (some lower? msg)))
            q
            (= (last msg) \?)]
        (cond
          (and all-cap q) "Calm down, I know what I'm doing!"
          all-cap "Whoa, chill out!"
          q "Sure."
          :else "Whatever.")))))
