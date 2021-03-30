#lang pie

(claim triple
  (Pi ((X U))
    (-> X
      (Pair X (Pair X X)))))
(define triple
  (lambda (X)
    (lambda (x)
      (cons x (cons x x)))))

(triple Atom 'a)
(triple Nat 1)
(triple (Pair Atom Atom) (cons 'a 'b))
