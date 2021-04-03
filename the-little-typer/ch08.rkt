#lang pie

(claim + (-> Nat Nat Nat))
(define +
  (lambda (m n)
    (iter-Nat m
      n
      (lambda (i) (add1 i)))))

(+ 23 45)

(claim +1=add1
  (Pi ((n Nat))
    (= Nat (+ 1 n) (add1 n))))
(define +1=add1
  (lambda (n)
    ;; the type for the following expression:
    ;; (= Nat (add1 n) (add1 n))
    ;; which is not exactly the same as
    ;; (= Nat (+ 1 n) (add1 n))
    ;; this can be proved simply by `same` as
    ;; (+ 1 n) and (add1 n) has the same normal form.
    ;; * simply change
    ;; > (= Nat (+ 1 n) (add 1 n))
    ;; to
    ;; > (= Nat (+ n 1) (add 1 n))
    ;; will make this fail.
    (same (add1 n))))

(claim incr (-> Nat Nat))
(define incr
  (lambda (n)
    (iter-Nat n
      1
      (+ 1))))

(claim incr=add1
  (Pi ((n Nat))
    (= Nat (incr n) (add1 n))))
(define incr=add1
  (lambda (n)
    (ind-Nat
      n
      (lambda (i) (= Nat (incr i) (add1 i)))
      (same 1)
      (lambda (n-1 r-incr=add1)
        ;; here we want to prove that (according to motive)
        ;; (incr n) and (add1 n) is the same.
        ;; side note: cong is like a map.
        (cong
          ;; we have proof that (incr n-1) and (add1 n-1) are same
          r-incr=add1
          ;; the following function must:
          ;; - transform (incr n-1) to (incr n)
          ;; - transform (add1 n-1) to (add1 n)
          (+ 1))))))