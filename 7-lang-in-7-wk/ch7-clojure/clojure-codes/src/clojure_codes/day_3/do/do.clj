(ns clojure-codes.day-3.do.do
  (:require 
    [clojure-codes.utils :as utils]))

; Task #1: use ref to create a group of accounts,
;     implement `credit` and `debit` for these accounts

(defn credit 
  [account-list who amount]
  (dosync
    (alter account-list 
           assoc who (+ amount (nth @account-list who)))))

(defn debit
  [account-list who amount]
  (credit account-list who (- amount)))

(defn -main [& args]
  (utils/eval-and-println
    ; create accounts
    (def account-list (ref [0 0 0]))
    @account-list

    (credit account-list 0 10)
    (debit account-list 0 1)
    (debit account-list 0 2)
    (debit account-list 1 10)
    (debit account-list 0 3)
    (credit account-list 2 20)
    (debit account-list 0 4)
    (debit account-list 2 20)
    (credit account-list 1 10)
  )
)
