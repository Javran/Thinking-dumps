;; primitives for the compiler

(load "./ec-prim.scm")

;; hiding some unused primitives
(define prompt-for-input #f)

(define cond?
  (list-tagged-with 'cond))

;; TODO: include common forms (maybe from ex-5.23)
