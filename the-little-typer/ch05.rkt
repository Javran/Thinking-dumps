#lang pie

(claim foo
  (List Atom))
(define foo
  (:: 'f (:: 'o (:: 'o nil))))
(claim bar
  (List Atom))
(define bar
  (:: 'b (:: 'a (:: 'r nil))))


(claim step-append
  (Pi ((E U))
    (-> E (List E) (List E) (List E))))
(define step-append
  (lambda (E)
    (lambda (hd tl r)
      ;; tl is not used below - it's already part of `r`.
      (:: hd r))))

(claim append
  (Pi ((E U))
    (-> (List E) (List E)
        (List E))))
(define append
  (lambda (E)
    (lambda (xs ys)
      (rec-List xs
        ys
        (step-append E)))))

(append Atom foo bar)
(append Atom bar bar)

(claim snoc
  (Pi ((E U))
    (-> (List E) E
        (List E))))
(define snoc
  (lambda (E)
    (lambda (xs y)
      (rec-List xs
        (:: y nil)
        (step-append E)))))

(snoc Atom
  (snoc Atom foo 'ee) 'ff)

(claim concat
  (Pi ((E U))
    (-> (List E) (List E)
        (List E))))
(define concat
  (lambda (E)
    (lambda (xs ys)
      (rec-List ys
        xs
        (lambda (hd tl r)
          (snoc E r hd))))))

(claim ns123
  (List Nat))
(define ns123
  (:: 1 (:: 2 (:: 3 nil))))

;; note: see https://thelittletyper.com/errata.html
;; this is indeed a mistake in the book, concat inserts elements from
;; second list but backwards.
(concat Nat ns123 ns123)

(claim reverse
  (Pi ((E U))
    (-> (List E) (List E))))
(define reverse
  (lambda (E)
    (lambda (xs)
      (rec-List xs
        (the (List E) nil)
        (lambda (hd tl r)
          (snoc E r hd))))))

(reverse Atom foo)