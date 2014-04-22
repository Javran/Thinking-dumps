(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

(define (install-amb-permanent-set!)

  (define (analyze-permanent-set! exp)
    (let ((var (assignment-variable exp))
          (vproc (amb-analyze (assignment-value exp))))
      (lambda (env succeed fail)
        (vproc env
               (lambda (val fail2)
                 (let ((old-value
                        (lookup-variable-value var env)))
                   (set-variable-value! var val env)
                   (succeed 'ok
                            ;; when the attempt is failed
                            ;; `set!` now restore the old value
                            fail2)))
               fail))))

  (define (test)
    (let ((env (init-env)))
    (do-test
     amb-eval-all
     (list
      ;; we only need to verify that
      ;; the side effect is permanently set.
      ;; which cannot be undone by calling the "fail" program
      (mat `(begin
              (define count 0)
              (amb 'a 'b 'c 'd)
              (permanent-set! count (+ count 1))
              count)
           env
           `(1 2 3 4))
           )))
    'ok)

  (define handler
    (make-amb-handler
     'permanent-set!
     analyze-permanent-set!
     test))

  (ahandler-register! handler)
  'ok)

(install-amb-permanent-set!)
(run-all-slot-tests)

(define (run-test-with symb-set)
  (out
   (amb-eval-all
    `(begin
       (define (not x)
         (if x #f #t))
       (define (require x)
         (if x 'pass (amb)))
       (define count 0)
       (let ((x (amb 'a 'b 'c))
             (y (amb 'a 'b 'c)))
         (,symb-set count (+ count 1))
         (require (not (eq? x y)))
         (list x y count)))
    (init-env))))

;; permanent-set! cannot be undone,
;; therefore "count" will keep counting
(run-test-with `permanent-set!)
;; however for "set!" every time before
;; a new candidate is about to be attempted,
;; the previous effect caused by "set!" is cancelled
;; causing "count" to stay at 1 and never get a chance
;; of increasing further.
(run-test-with `set!)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
