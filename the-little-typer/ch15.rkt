#lang pie

(claim =consequence (-> Nat Nat U))
(define =consequence
  (lambda (x y)
    (which-Nat x
      ;; when x is zero
      (which-Nat y
        ;; when y is also zero
        ;; I'd say (= Nat 0 0) could be a consequence
        ;; which should be as good as Trivial
        ;; since both types have exactly one value.
        Trivial
        ;; when y is non-zero
        (lambda (y-1)
          ;; No value for this type - since 0 cannot equal some (add1 _).
          Absurd))
      ;; when x is non-zero
      (lambda (x-1)
        (which-Nat y
          ;; when y is zero
          Absurd
          ;; when y is non-zero
          (lambda (y-1)
            ;; by knowing some `(add1 x-1)` is equal to some `(add1 y-1)`,
            ;; we learn that `x-1` and `y-1` should be the same.
            (= Nat x-1 y-1)))))))

;; looks foolish but remember it's not possible to find a proof for it.
;; (this is as good as absurd)
(=consequence 3 5)
;; Absurd but directly.
(=consequence 1 0)
;; but we can prove this:
(the (=consequence 2 2) (same 1))

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
(=consequence-same 2)

(claim use-Nat=
  (Pi ((x Nat)
       (y Nat))
    (-> (= Nat x y)
        (=consequence x y))))
(define use-Nat=
  (lambda (x y x=y)
    ;; I'm wondering if we can use ind-Nat here
    ;; but making good use of x=y is better.
    (replace x=y
      ;; I don't think it matters which one to replace which one,
      ;; since there are only 2 choices and one can always use `symm`.
      (lambda (ky) (=consequence x ky))
      (=consequence-same x))))
(claim zero-not-add1
  (Pi ((n Nat))
    (-> (= Nat zero (add1 n)) Absurd)))
(define zero-not-add1
  (lambda (n 0=1+n)
    (use-Nat= 0 (add1 n) 0=1+n)))

(claim donut-absurdity
  (-> (= Nat 0 6) (= Atom 'powdered 'glazed)))
(define donut-absurdity
  (lambda (0=6)
    (ind-Absurd (use-Nat= 0 6 0=6)
      (= Atom 'powdered 'glazed))))

(claim sub1=
  (Pi ((x Nat)
       (y Nat))
    (-> (= Nat (add1 x) (add1 y))
        (= Nat x y))))
(define sub1=
  (lambda (x y x+1=y+1)
    (use-Nat= (add1 x) (add1 y) x+1=y+1)))
(claim one-not-six
  (-> (= Nat 1 6) Absurd))
(define one-not-six
  (lambda (1=6)
    (use-Nat= 0 5
      (sub1= 0 5 1=6))))


(claim front
  (Pi ((E U)
       (n Nat))
    (-> (Vec E (add1 n)) E)))
#;
(define front
  (lambda (E n xs)
    (ind-Vec (add1 n) xs
      (lambda (_ _) E)
      ;; note that this is a `vecnil` case, which is impossible
      ;; by construction.
      ;; I'd argue this is a valid implementation since `(head xs)`
      ;; in this context is safe to obtain an `E`,
      ;; and this is a unreachable branch anyway.
      (head xs)
      (lambda (_ hd _ _) hd))))

(define front
  (lambda (E n xs)
    ((ind-Vec (add1 n) xs
       ;; this is a tricky motive as we are passing in
       ;; two extra arguments from outside,
       ;; namely y and a proof of kn=1+y
       (lambda (kn kxs)
         (Pi ((y Nat))
           (-> (= Nat kn (add1 y)) E)))
       ;; instead of offering a value,
       ;; this `vecnil` branch proves its absurdity
       ;; since 0 cannot equal to any value that has `add1` at top.
       (lambda (y 0=1+y)
         (ind-Absurd (zero-not-add1 y 0=1+y) E))
       (lambda (k hd tl r y k=y) hd))
      n (same (add1 n)))))

(front Nat 2 (vec:: 1 (vec:: 2 (vec:: 3 vecnil))))
(front Nat 0 (vec:: 12 vecnil))

;; are we basically proving double negation?
(claim pem-not-false
  (Pi ((X U))
    (-> (-> (Either X (-> X Absurd))
            Absurd)
        Absurd)))
(define pem-not-false
  (lambda (X pem-false)
    (pem-false
      ;; we must go down `right` here
      ;; as we don't have any value of type X available.
      (right
        (lambda (x)
          ;; well, now we do have `(the X x)`
          (pem-false (left x)))))))

(claim Dec (-> U U))
(define Dec
  (lambda (X)
    ;; A statement X is decidable if it's either true or false.
    (Either X (-> X Absurd))))