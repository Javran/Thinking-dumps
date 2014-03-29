(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./my-eval.scm")

;; unless as a special form
(define (install-eval-unless)

  (define (unless->if exp)
    (let ((pred-exp (cadr exp))
          (conseq-exp (caddr exp))
          (alter-exp (cadddr exp)))
      `(if ,pred-exp ,alter-exp ,conseq-exp)))

  (define (eval-unless exp env)
    (my-eval (unless->if exp) env))

  (define (analyze-unless exp)
    (my-analyze (unless->if exp)))

  (define (test-eval eval-unless)
    (let ((env (init-env)))
      (do-test
       eval-unless
       (list
        (mat `(unless #t 10 20) env 20)
        (mat `(unless #f 30 40) env 30))))
    'ok)

  (define handler
    (make-handler
      'unless
      eval-unless
      analyze-unless
      (test-both
       test-eval
       eval-unless
       analyze-unless)))

  (handler-register! handler)
  'ok)

(install-eval-unless)

(my-eval-test-installed-handlers)

;; I don't personally think a procedure-like `unless`
;; is useful, so unfortunately here
;; I can only come up with some far-fetched usages for
;; `unless` as a procedure.

;; Suppose there is a data structure like:
;;   ((#t #f ...)
;;    ( 1  2 ...)
;;    ( 3  4 ...))
;; and I'd like to convert each element using the rule:
;; * transpose this structure as if it was a matrix
;; * if the first element is true, just keep the third one
;; * otherwise, just keep the second one
;; this can be easily implemented using `fold-left`.

;; this proc should be written in normal-order languages
;; but in this case, since the outcome is the same,
;; for simplicity, I just ignore this fact.
(define (unless1 p a b)
  (if p b a))

(out (map
      unless1
      '(#t #f #t)
      '(10 20 30)
      '(40 50 60)))
;; 40 20 60

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
