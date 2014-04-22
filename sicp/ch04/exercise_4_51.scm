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
    'todo)

  (define handler
    (make-amb-handler
     'permanent-set!
     analyze-permanent-set!
     test))

  (ahandler-register! handler)
  'ok)

(install-amb-permanent-set!)
(run-all-slot-tests)

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
       (permanent-set! count (+ count 1))
       (require (not (eq? x y)))
       (list x y count)))
  (init-env)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
