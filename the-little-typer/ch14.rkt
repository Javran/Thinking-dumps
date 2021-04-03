#lang pie

(the Trivial sole)

(claim Maybe (-> U U))
(define Maybe
  (lambda (E)
    ;; the choice of making left part "Just" is weird...
    ;; but well, that doesn't matter too much here.
    (Either E Trivial)))
(claim nothing (Pi ((E U)) (Maybe E)))
(define nothing
  (lambda (_) (right sole)))
(claim just (Pi ((E U)) (-> E (Maybe E))))
(define just
  (lambda (_ x) (left x)))

(nothing Atom)
(just Atom 'z)

(claim sample
  (List Atom))

(define sample
  (:: 's
    (:: 'a
      (:: 'm
        (:: 'p
          (:: 'l
            (:: 'e
              nil)))))))

(claim maybe-head
  (Pi ((E U))
    (-> (List E)
        (Maybe E))))
(define maybe-head
  (lambda (E xs)
    (rec-List xs
      (nothing E)
      (lambda (hd _ _)
        (just E hd)))))
(maybe-head Atom nil)
(maybe-head Atom sample)

(claim maybe-tail
  (Pi ((E U))
    (-> (List E)
        (Maybe (List E)))))
(define maybe-tail
  (lambda (E xs)
    (rec-List xs
      (nothing (List E))
      (lambda (_ tl _)
        (just (List E) tl)))))
(maybe-tail Atom nil)
(maybe-tail Atom sample)

(claim list-ref
  (Pi ((E U))
    (-> Nat (List E) (Maybe E))))
(define list-ref
  (lambda (E i)
    (ind-Nat i
      (lambda (_) (-> (List E) (Maybe E)))
      (maybe-head E)
      (lambda (i-1 r xs)
        (ind-Either (maybe-tail E xs)
          (lambda (_) (Maybe E))
          r
          (lambda (_) (nothing E)))))))
(list-ref Atom 3 sample)
(list-ref Atom 5 sample)
(list-ref Atom 6 sample)

(claim Fin
  (-> Nat U))
(define Fin
  (lambda (n)
    (iter-Nat n
      Absurd
      Maybe)))
(Fin 2)
(nothing (Fin 2))
(just (Fin 2) (nothing (Fin 1)))

(claim fzero
  ;; the zero index of a Vec of length n+1
  ;; (since a Vec of length 0 contains no value)
  (Pi ((n Nat))
    (Fin (add1 n))))
(define fzero
  (lambda (n)
    (nothing (Fin n))))
(claim fadd1
  (Pi ((n Nat))
    (-> (Fin n)
        (Fin (add1 n)))))
(define fadd1
  (lambda (n i-1)
    (just (Fin n) i-1)))
;; observation:
;; like 0 and add1,
;; - fzero has "right" on top
;; - fadd1 has "left" on top

;; those are the exact same two values as before:
(fzero 2)
(fadd1 2 (fzero 1))

(claim vec-ref
  (Pi ((E U)
       (l Nat))
    (-> (Fin l) (Vec E l) E)))
(define vec-ref
  (lambda (E l)
    (ind-Nat l
      (lambda (k) (-> (Fin k) (Vec E k) E))
      (lambda (a _) (ind-Absurd a E))
      (lambda (_ r index xs)
        (ind-Either index
          (lambda (_) E)
          ;; fadd
          (lambda (index-1)
            (r index-1 (tail xs)))
          ;; fzero
          (lambda (_) (head xs)))))))
(claim example
  (Vec Atom 7))
(define example
  (vec:: 'e
    (vec:: 'x
      (vec:: 'a
        (vec:: 'm
          (vec:: 'p
            (vec:: 'l
              (vec:: 'e
                vecnil))))))))
(vec-ref Atom 7 (fzero 6) example)
(vec-ref Atom 7 (fadd1 6 (fzero 5)) example)
(vec-ref Atom 7 (fadd1 6 (fadd1 5 (fzero 4))) example)