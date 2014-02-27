(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (force-it obj)
  (if (thunk? obj)
      (eval (thunk-exp obj)
                  (thunk-env obj))
      obj))

(define (delay-it exp env)
  `(thunk ,exp ,env))

(define (thunk? obj)
  (and (non-empty? obj)
       (eq? (car obj) 'thunk)))

(define thunk-exp cadr)
(define thunk-env caddr)

(define delayed-val
  (delay-it
   `(begin
      (display "evaluating...")
      (newline)
      (+ 1 2 3 4))
   user-initial-environment))

;; a delayed value,
;; a computation with its environment
(out delayed-val)

;; a thunk without memoization
;; usually call-by-name strategy is implemented
;; in this way.
;; whenever we force the value,
;; the expression get evaluated.
;; we can observe this by causing a side effect
;; (in this case, `display` and `newline`)
;; inside the delayed expression.
(out (force-it delayed-val))
(out (force-it delayed-val))
;; we will observer two `evaluating...`
;; are outputed here.

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
