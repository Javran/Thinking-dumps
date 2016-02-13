(ns logical.day1-hard)

(use 'clojure.core.logic)
(use 'clojure.core.logic.pldb)
(use 'logical.utils)

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

