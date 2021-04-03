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