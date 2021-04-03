#lang pie

(claim double (-> Nat Nat))
(define double
  (lambda (n)
    (iter-Nat n
      0
      (lambda (r) (add1 (add1 r))))))

(claim + (-> Nat Nat Nat))
(define +
  (lambda (m n)
    (iter-Nat m
      n
      (lambda (i) (add1 i)))))

(claim Even (-> Nat U))
(define Even
  (lambda (n)
    (Sigma ((half Nat))
      (= Nat n (double half)))))
(claim Odd (-> Nat U))
(define Odd
  (lambda (n)
    (Sigma ((haf Nat))
      (= Nat n (add1 (double haf))))))

(claim add1-even->odd
  (Pi ((n Nat))
    (-> (Even n)
        (Odd (add1 n)))))
(define add1-even->odd
  (lambda (_ ep)
    (cons (car ep) (cong (cdr ep) (+ 1)))))
(claim add1-odd->even
  (Pi ((n Nat))
    (-> (Odd n)
        (Even (add1 n)))))
(define add1-odd->even
  (lambda (_ op)
    (cons (add1 (car op)) (cong (cdr op) (+ 1)))))


(claim even-or-odd
  (Pi ((n Nat))
    (Either (Even n) (Odd n))))
(define even-or-odd
  (lambda (n)
    (ind-Nat n
      (lambda (k)
        (Either (Even k) (Odd k)))
      (left (cons 0 (same 0)))
      (lambda (n-1 n-1-is-even-or-odd)
        (ind-Either n-1-is-even-or-odd
          (lambda (_) (Either (Even (add1 n-1)) (Odd (add1 n-1))))
          ;; n-1 is Even
          (lambda (p) (right (add1-even->odd n-1 p)))
          ;; n-1 is Odd
          (lambda (p) (left (add1-odd->even n-1 p))))))))
(even-or-odd 3)
(even-or-odd 10)    