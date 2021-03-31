#lang pie

(claim abc (Vec Atom 3))
(define abc
  (vec:: 'a (vec:: 'b (vec:: 'c vecnil))))

;; trying to write a function that takes first two elements from a Vec
;; of sufficient length.
(claim vec-take2
  (Pi ((X U)
       (l Nat))
    (-> (Vec X
          ;; this thing really feels like a trick rather than
          ;; properly writing a constraint - one can tell by
          ;; how l is given in a unintuitive way -
          ;; any way that this issue can be mitigated?
          (add1 (add1 l)))
        (Vec X 2))))
(define vec-take2
  (lambda (X l)
    (lambda (xs)
      (vec:: (head xs)
        (vec:: (head (tail xs))
          vecnil)))))

;; note that there is no type inference so we need to
;; give it an appropriate length.
(vec-take2 Atom 1 abc)