(ns logical.day1-hard)

(use 'clojure.core.logic)
(use 'clojure.core.logic.pldb)
(use 'logical.utils)

;; c is p's child
;; image it as p -> c
;; where "->" means gives birth to or something
(db-rel childo p c)
;; p2 is p1's wife
(db-rel wifeo p1 p2)

(defn spouseo [p1 p2]
  (conde
   [(wifeo p1 p2)]
   [(wifeo p2 p1)]))

;; p2 is p1's ancestor
;; notation: p1 <~~ p2
(defn ancestoro [p1 p2]
  (conde
   [
    ;; base case: if p1 is p2' child
    ;; then p2 is p1's ancestor
    ;; notation:
    ;; p2 -> p1 then p1 <~~ p2
    (childo p2 p1)]
   [;; recursive: if p1 is p2' child
    ;; and p3 is p2's ancestor
    ;; then p3 is also p1's ancestor
    ;; notation:
    ;; p2 -> p3 and p1 <~~ p3
    ;; then p1 <~~ p2
    (fresh [p3]
      (childo p2 p3)
      (ancestoro p1 p3))]))

(def genealogy-facts
  ;; see: SICP ex 4.63 xD
  ;; TODO: these relations are very unintuitive to read
  ;; we need another example to replace this
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

(defn day1-hard
  []
  (p "day 1 - do hard")
  (p "exercise 1")

  (p
   (with-db genealogy-facts
     (run* [p2]
       (ancestoro :cain p2))))
  )
