#lang pie


(claim step-+ (-> Nat Nat))
(define step-+
  ;; Pie seems unhappy if I just use add1 directly - so `add1` is not a function?
  (lambda (k) (add1 k)))

(claim + (-> Nat Nat Nat))
(define +
  (lambda (m n)
    (iter-Nat m
      n
      step-+)))

;; (the Nat 9)
(+ 2 7)
(+ (+ 1 (+ 3 2)) 3)

(claim gauss (-> Nat Nat))
(define gauss
  (lambda (n)
    (rec-Nat n
      0
      (lambda (n-1 r)
        (+ (add1 n-1) r)))))

;; (the Nat 210)
(gauss 20)

(claim * (-> Nat Nat Nat))
(define *
  (lambda (m n)
    (rec-Nat n
      0
      ;; just to demonstrate here that lambda are curried in Pie,
      ;; (lambda (x y) ...) is the same as:
      ;; (lambda (x) (lambda (y) ...))
      (lambda (n-1)
        (lambda (r)
          (+ m r))))))

;; (the Nat 720)
(* (* (* 3 4) (* 6 5)) 2)