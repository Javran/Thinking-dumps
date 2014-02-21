; handle and-exp
; from ./exercise 4_4_a.scm

(define (install-eval-and)
  (define (eval-and exp env)
    ; evaluate expressions, short circuit if necessary
    (define (eval-and-aux exps env)
      (cond ((null? exps) #t)
            ((null? (cdr exps))
             ; we are dealing with the last element in the list
             ; simply evaluating it will do
             (my-eval (car exps) env))
            (else
              (if (true? (my-eval (car exps) env))
                ; need further examination
                (eval-and-aux (cdr exps) env)
                ; else short circuit
                #f))))
    (eval-and-aux (cdr exp) env))

  (define (analyze-and exp)
    (define exps (cdr exp))
    ;; we are just doing analysis,
    ;; so don't care too much about efficiency
    (define analyzed-exps (map my-analyze exps))

    ;; analyze-aux :: [Analyzed] -> Analyzed
    (define (analyze-aux analyzed-exps)
      (if (null? analyzed-exps)
          ;; (and) => #t
          ;; (and x (and) => (and x #t) => x
          (const #t)
          ;; the benefit of `analyze` is that
          ;; we only do the analyze once,
          ;; and then store it somewhere.
          (let ((hd (car analyzed-exps))
                (tl (cdr analyzed-exps)))
            (if (null? (cdr analyzed-exps))
                ;; a list containing exactly one element
                hd
                ;; otherwise, do evaluation as needed.
                (lambda (env)
                  (if (true? (hd env))
                      ;; keep going
                      ((analyze-aux tl) env)
                      #f))))))

    (analyze-aux analyzed-exps))

  (define (test-eval eval-and)
    (let ((env (init-env)))
      (do-test
        eval-and
        (list
          (mat '(and) env #t)
          (mat '(and (= 1 1) (= 2 2)) env #t)
          (mat '(and (= 1 1) #f (error 'wont-reach)) env #f)
          (mat '(and 1 2 3 4) env 4)
          (mat '(and 1) env 1)
          ))
      'ok))

  (define handler
    (make-handler
      'and
      eval-and
      analyze-and
      (test-both
       test-eval
       eval-and
       analyze-and)))

  (handler-register! handler)
  'ok)
;; Local variables:
;; proc-entry: "./my-eval.scm"
;; End:
