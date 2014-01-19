#lang eopl

(require "../common.rkt")

(provide do-env-test)

; feed `do-env-test` with
;   3 interface implementations for environment.
;   to test for correctness
(define (do-env-test
          empty-env
          extend-env
          apply-env)
  (define env
    (my-foldl (lambda (env pair)
             (extend-env
               (car pair)
               (cadr pair)
               env))
           (empty-env)
           '((x 1)
             (y 2)
             (x 3)
             (z 4)
             (y 5)
             (z a))))
  (define (expect-k-v k v)
    ; test the correctness of the environment
    ;   if `k` is given, `v` should be returned,
    ;   elsewise an exception will be raised.
    (let ((result (apply-env env k)))
      (if (eqv? result v)
        'done
        (eopl:error
          'do-env-test
          "Query for `~s` should return `~s`, but the actual output is `~s`"
          k v result))))
  (expect-k-v 'x 3)
  (expect-k-v 'y 5)
  (expect-k-v 'z 'a)
  (out "env test passed.")
  )
