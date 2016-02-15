(ns logical.day1-hard)

(use 'clojure.core.logic)
(use 'clojure.core.logic.pldb)
(use 'logical.utils)

;; the genelogy-facts example is taken from SICP ex 4.63 XD
;; I think the original relation is hard to read:
;; you don't know if (childo a b) means "(one of) a's child is b" or
;; "a is b's child". therefore in the following part we will introduce
;; some notations to make things easier to read.

;; c is p's child
;; image it as p -> c
;; we will use notation "->", imagine
;; it means gives birth to or something
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

(defn descendanto [p1 p2]
  (ancestoro p2 p1))

(def genealogy-facts
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

(defn extendo [xs ys rs]
  (conde
   [
    (== xs [])
    (== ys rs)]
   [(fresh [hd tl rs1]
      (conso hd tl xs)
      (extendo tl ys rs1)
      (conso hd rs1 rs))]))

(defn day1-hard
  []
  (p "day 1 - do hard")
  (p "exercise 1")

  (p "find Cain's ancestors")
  (p
   (with-db genealogy-facts
     (run* [p2]
       (ancestoro :cain p2))))
  (p "find Lamech's ancestors")
  (p
   (with-db genealogy-facts
     (run* [p2]
       (ancestoro :lamech p2))))
  (p "find Jubal's ancestors")
  ;; with our limited rules, the system cannot deduce that
  ;; Jubal is also Lamech's child so the ancestors
  ;; originated from Lamech are all missing.
  ;; to fix this problem, we either need to come up with
  ;; some helper rules or just put the conclusion into db.
  ;; for now we just live with it.
  (p
   (with-db genealogy-facts
     (run* [p2]
       (ancestoro :jubal p2))))
  (p "find Cain's descendants")
  (p
   (with-db genealogy-facts
     (run* [p]
       (descendanto :cain p))))
  ;; TODO: skipping cousin relation because I don't know how to negate.
  ;; basically if a and b are cousins, their parents are different
  ;; but their parents' parent is the same
  ;; and to encode this rule, one needs negation
  )

