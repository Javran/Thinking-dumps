(ns logical.day1-medium)

(use 'clojure.core.logic)
(use 'clojure.core.logic.pldb)
(use 'logical.utils)
(use 'logical.day1-book)

(defn scientisto [p]
  (conde
   [(mano p)]
   [(womano p)]))

;; c is p's child
(db-rel childo p c)
;; p2 is p1's wife
(db-rel wifeo p1 p2)

(defn spouseo [p1 p2]
  (conde
   [(wifeo p1 p2)]
   [(wifeo p2 p1)]))

(def genealogy-facts
  ;; see: SICP ex 4.63 xD
  (db
   [childo :adam :cain]
   [childo :cain :enoch]
   [childo :enoch :irad]
   [childo :irad :mehujael]
   [childo :mehujael :methushael]
   [childo :methushael :lamech]
   [childo :ada :jabal]
   [childo :ada :jubal]

   [wifeo :lamech :ada]))

(defn day1-medium
  []
  (p "day 1 - do medium")
  (p "exercise 1")
  (p
   (with-db facts
     (run* [p]
       (scientisto p))))
  (p "exercise 2")
  ;; find all scientists who've won Turing Awards
  (p
   (with-db facts
    (run* [p]
      (fresh [y]
        (scientisto p)
        (turingo p y))))))
