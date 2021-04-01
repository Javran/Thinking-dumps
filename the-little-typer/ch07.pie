#lang pie

(claim make-vector
  (Pi ((X U)
       (e X)
       (count Nat))
    (Vec X count)))

(define make-vector
  (lambda (X e count)
    (ind-Nat
      ;; target value to analyze (perform induction on)
      count
      ;; motive, from that number to a type (the return type)
      (lambda (l)
        (Vec X l))
      ;; base case (zero), l=0 so we only have one construct:
      vecnil
      ;; step function - we can simply prepend to the recursive result.
      (lambda (l-1 r)
        (vec:: e r)))))

(claim peas
  (Pi ((count Nat))
    (Vec Atom count)))

(define peas
  (make-vector Atom 'pea))

(make-vector Nat 123 4)
(peas 6)
(make-vector (Vec Atom 3) (peas 3) 4)

(claim thing
  (Vec Atom 5))
(define thing
  (vec:: 't (vec:: 'h (vec:: 'i (vec:: 'n (vec:: 'g vecnil))))))

(claim last
  (Pi ((X U)
       (l Nat))
    (-> (Vec X (add1 l))
        X)))

;; note that all inner `(the _ a)` are not necessary and can be replaced by
;; just `a`.
(define last
  (lambda (X l)
    (ind-Nat
      ;; target
      l
      ;; motive
      (lambda (i)
        (-> (Vec X (add1 i)) X))
      ;; base.
      ;; this type signature is not necessary,
      ;; just to demonstrate what's going on.
      (the
        (-> (Vec X 1) X)
        (lambda (xs) (head xs)))
      ;; step
      (lambda (l-1 r xs)
        ;; here r is a function that deals with "smaller case",
        ;; and we need to discard first element from xs
        ;; to make the input for r.
        ((the (-> (Vec X (add1 l-1)) X) r)
          (tail xs))))))

(last Atom 4 thing)

(claim drop-last
  (Pi ((X U)
       (l Nat))
    (-> (Vec X (add1 l))
        (Vec X l))))
(define drop-last
  (lambda (X l)
    (ind-Nat
      ;; target
      l
      ;; motive
      (lambda (i)
        (-> (Vec X (add1 i))
            (Vec X i)))
      ;; base
      (lambda (_) vecnil)
      ;; step
      (lambda (l-1 r xs)
        (vec:: (head xs)
          (r (tail xs)))))))
(drop-last Atom 4 thing)