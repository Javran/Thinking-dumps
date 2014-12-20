;; effectively undefining "cond->if"
(define cond->if #f)

(define cond-clauses cdr)
(define first-clause car)
(define clause-cond car)
(define (single-clause? xs)
  (and (pair? xs)
       (null? (cdr xs))))
(define clause-actions cdr)
