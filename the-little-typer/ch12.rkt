#lang pie

(claim double (-> Nat Nat))
(define double
  (lambda (n)
    (iter-Nat n
      0
      (lambda (r) (add1 (add1 r))))))

(claim Even (-> Nat U))
(define Even
  (lambda (n)
    ;; an even Nat is double of some other Nat.
    (Sigma ((half Nat))
      (= Nat n (double half)))))
;; note that (Even _) produces a type:
(Even 2)
(Even 3)

;; but the real question is whether that type has any resident.
(the (Even 2) (cons 1 (same 2)))
;; however it won't be possible to construct a value of type (Even 3)

;; side note: we prefer `double` over `twice` despite that
;; we have proved that both function produce the same result
;; for same input, this is because (quoting from book):
;; > sometimes one of them is easier to use because
;; > it more quickly becomes a value.

(claim + (-> Nat Nat Nat))
(define +
  (lambda (m n)
    (iter-Nat m
      n
      (lambda (i) (add1 i)))))

(claim +two-even
  (Pi ((n Nat))
    (-> (Even n)
        (Even (+ 2 n)))))

(define +two-even
  (lambda
    ;; don't use first argument to replace (car en) below,
    ;; this is because the `cdr` expects the corresponding `car` part.
    ;; I suspect using the first argument is possible but
    ;; not very convenient.
    (_ en)
    (cons (add1 (car en)) (cong (cdr en) (+ 2)))))

;; use proof of (Even 10) to produce a proof of (Even 12)
(+two-even 10 (cons 5 (same 10)))

(claim Odd (-> Nat U))
(define Odd
  (lambda (n)
    (Sigma ((haf Nat))
      (= Nat n (add1 (double haf))))))
(the (Odd 7) (cons 3 (same 7)))

(claim add1-even->odd
  (Pi ((n Nat))
    (-> (Even n)
        (Odd (add1 n)))))
(define add1-even->odd
  (lambda (_ ep)
    (cons (car ep) (cong (cdr ep) (+ 1)))))
(the (Odd 9) (add1-even->odd 8 (cons 4 (same 8))))

(claim add1-odd->even
  (Pi ((n Nat))
    (-> (Odd n)
        (Even (add1 n)))))
(define add1-odd->even
  (lambda (_ op)
    (cons (add1 (car op)) (cong (cdr op) (+ 1)))))
(the (Even 14) (add1-odd->even 13 (cons 6 (same 13))))