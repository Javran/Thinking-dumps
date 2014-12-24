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

;; now we slight change the implementations..
(define (evaluated-thunk? obj)
  (and (non-empty? obj)
       (eq? (car obj) 'evaluated-thunk)))

(define thunk-value cadr)

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (eval (thunk-exp obj)
                             (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj)
                     result)
           (set-cdr! (cdr obj)
                     ;; I think this is just
                     ;; an optimization for gc
                     '())
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

;; let's redo all the tests here
(newline) (newline)
(define delayed-val
  (delay-it
   `(begin
      (display "evaluating...")
      (newline)
      (+ 1 2 3 4))
   user-initial-environment))

(out delayed-val)

;; now the thunk is equipped with
;; memoization, this means the side effects
;; are one-shot, the next time we force a value,
;; we will get the previous result.
(out (force-it delayed-val))
(out (force-it delayed-val))
;; we can only observer one `evaluating...` now.

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
