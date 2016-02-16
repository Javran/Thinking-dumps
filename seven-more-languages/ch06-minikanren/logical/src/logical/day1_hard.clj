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
   [;; case 1: when xs is []
    (== xs [])
    (== ys rs)]
   [;; case 2: when xs is non-empty
    (fresh [hd tl]
      ;; xs = (cons hd tl)
      (conso hd tl xs)
      ;; result's head is hd, with rest of it
      ;; concatenated with ys
      (fresh [rs1]
        ;; make sure to put "conso" before recursive "extendo"
        ;; the reason of this is to make recursion "productive"
        ;; when xs is unknown.
        ;;
        ;; imagine when "xs" is unknown, both branch of "conde" will
        ;; be attempted, however, when the program searches through
        ;; this point, if "extendo" follows immediately,
        ;; we will be going into a new loop, and no extra information
        ;; about "xs" is known, so we end up with an infinite loop
        ;; so despite the book says "order does not matter", it's a lie,
        ;; the order matters and sometimes we need to think carefully about it.
        (conso hd rs1 rs)
        (extendo tl ys rs1)))]))

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

  (p "exercise 2")
  ;; direction 1: result is unknown
  (p
   (run* [q]
       (extendo [1 2 3] [4 5 6] q)))
  ;; direction 2: 2nd arg is unknown
  (p
   (run* [q]
       (extendo [1 2 3] q [1 2 3 3 2 1])))
  ;; direction 3: 1st arg is unknown
  ;; TODO: for now it's not working
  (p
   (run* [q]
     (extendo q [4 5 6] [2 2 4 5 6])))
  )

