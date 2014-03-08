;; this is the implementation of `promise`,
;; as an extension of `my-eval`

;; a promise is a list with the first element
;; being either symbol `promise` or symbol `promise-result`
;; * if the symbol is `promise`, the rest of the list
;;   should contain all arguments necessary for an `eval` procedure
;; * if the symbol is `promise-result`, the rest of the list
;;   should be a singleton list that contains the evaluated result.
(define (make-promise exp env)
  `(promise ,exp ,env))

(define (promise? obj)
  (and (non-empty? obj)
       (memq (car obj)
             '(promise promise-result))))

(define (promise-force promise)
  (case (car promise)
    ((promise)
     ;; do the evaluation, and store the result.
     (let ((result (apply my-eval (cdr promise))))
       result
       (set-car! promise 'promise-result)
       ;; keep the structure being a list.
       (set-cdr! promise (list result))
       result))
    ((promise-result)
     ;; a cached value
     (cadr promise))
    (else
     (error "Not a promise"
            promise))))

(define (test-promise)
  (define env (init-env))
  ;; counter for laziness test (by observing side effect)
  (my-eval `(define count 0) env)

  ;; correctness test
  (do-test
   (lambda (exp)
     (promise-force
      (make-promise
       exp env)))
   (list
    (mat `(+ 1 2 3 4) 10)
    (mat `(- 1 2) -1)
    (mat `((lambda (a) (* a a)) 12) 144)
    ))

  ;; memoization test
  (define p (make-promise
             `(+ 1 2 3 4)
             env))

  ;; original structure
  (assert (promise? p))
  (assert (eq? (car p) 'promise))

  (assert (eq? (promise-force p) 10))

  ;; new structure verification
  (assert (promise? p))
  (assert (equal? p '(promise-result 10)))

  ;; force on cached value
  (assert (eq? (promise-force p) 10))
  (assert (equal? p '(promise-result 10)))

  ;; laziness test, by observing side effects
  (define pse
    ;; promise with side effects
    (make-promise
     `(begin
        (set! count (+ count 1))
        10)
     env))

  (assert (= (lookup-variable-value
              'count
              env)
             0))

  ;; do evaluation multiple times
  (assert (eq? (promise-force pse) 10))
  (assert (eq? (promise-force pse) 10))
  (assert (eq? (promise-force pse) 10))

  ;; the side effect should happen
  ;; exactly once
  (assert (= (lookup-variable-value
              'count
              env)
             1))

  'ok)

;; Local variables:
;; proc-entry: "./exercise_4_31.scm"
;; End:
