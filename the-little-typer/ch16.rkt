#lang pie

(claim Dec (-> U U))
(define Dec (lambda (X) (Either X (-> X Absurd))))

(claim =consequence (-> Nat Nat U))
(define =consequence
  (lambda (x y)
    (which-Nat x
      (which-Nat y
        Trivial
        (lambda (y-1)
          Absurd))
      (lambda (x-1)
        (which-Nat y
          Absurd
          (lambda (y-1)
            (= Nat x-1 y-1)))))))
(claim =consequence-same
  (Pi ((n Nat))
    (=consequence n n)))
(define =consequence-same
  (lambda (n)
    (ind-Nat n
      (lambda (k) (=consequence k k))
      sole
      (lambda (n-1 _)
        (same n-1)))))
(claim use-Nat=
  (Pi ((x Nat)
       (y Nat))
    (-> (= Nat x y)
        (=consequence x y))))
(define use-Nat=
  (lambda (x y x=y)
    (replace x=y
      (lambda (ky) (=consequence x ky))
      (=consequence-same x))))
(claim zero-not-add1
  (Pi ((n Nat))
    (-> (= Nat zero (add1 n)) Absurd)))
(define zero-not-add1
  (lambda (n 0=1+n)
    (use-Nat= 0 (add1 n) 0=1+n)))

(claim zero?
  (Pi ((n Nat))
    (Dec (= Nat zero n))))
(define zero?
  (lambda (n)
    (ind-Nat n
      (lambda (k) (Dec (= Nat zero k)))
      (left (same 0))
      (lambda (n-1 _)
        (right (zero-not-add1 n-1))))))

(claim + (-> Nat Nat Nat))
(define +
  (lambda (m n)
    (iter-Nat m
      n
      (lambda (i) (add1 i)))))

(claim dec-add1=
  (Pi ((x Nat)
       (y Nat))
    (-> (Dec (= Nat x y)) (Dec (= Nat (add1 x) (add1 y))))))
(define dec-add1=
  (lambda (x y dec-x=y)
    (ind-Either dec-x=y
      (lambda (_) (Dec (= Nat (add1 x) (add1 y))))
      ;; when equal
      (lambda (x=y)
        (left (cong x=y (+ 1))))
      ;; when not equal
      (lambda (x=y-false)
        (right
          ;; the goal is to establish absurdity
          ;; through a proof that x=y
          (lambda (x=y)
            (x=y-false
              (use-Nat= (add1 x) (add1 y) x=y))))))))

(claim nat=?
  (Pi ((x Nat)
       (y Nat))
    (Dec (= Nat x y))))
(define nat=?
  (lambda (x y)
    ((ind-Nat x
       (lambda (kx)
         ;; the trick here is that `j` is passed from outside
         ;; as we want to build proof from `(_ x-1 j-1)` to `(_ x j)`,
         ;; and `y` does not have the desired shape.
         (Pi ((j Nat))
           (Dec (= Nat kx j))))
       ;; when x = 0, it's decidable through `zero?`
       zero?
       (lambda (x-1 nat=?-x-1 j)
         ;; induction on `j`, we want to work on `j=0`
         ;; and on the other front work from `(_ x-1 j-1)`
         ;; to build `(_ x j)`
         ;; or `(_ (add1 x-1) (add1 j-1))`
         (ind-Nat j
           (lambda (k) (Dec (= Nat (add1 x-1) k)))
           ;; nonzeros are, well, not zero.
           (right (use-Nat= (add1 x-1) 0))
           (lambda (j-1 _dec)
             (dec-add1=
               x-1
               j-1
               ;; obtain the proof that (= Nat x-1 j-1) is decidable.
               (nat=?-x-1 j-1))))))
      y)))

;; The proof that (= Nat x y) is decidable also serves
;; the purpose of equality check.
;; right => not equal
(nat=? 2 1)
;; left => equal
(nat=? 5 5)