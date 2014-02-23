(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./my-eval.scm")

(define test-exp
  `(begin
     (define (fib n)
       (if (<= n 1)
           n
           (+ (fib (- n 1))
              (fib (- n 2)))))
     (fib 20)))

(define env (init-env))

(for-each
 (lambda (a)
   (my-eval-select-approach a)
   (format #t "answer: ~A~%"
           (time-test my-eval test-exp env)))
 '(analyze interpret))

;; some results:
;; when the first approach tested is `interpret`
;; Time elapsed: ~ 3280 (interpret)
;; Time elapsed: ~ 1696 (analyze)
;; when the first approach tested is `analyze`
;; Time elapsed: ~ 3138 (interpret)
;; Time elapsed: ~ 1817 (analyze)
;; seems startup test consumes more time,
;; but it's clear `analyze` offers a much better
;; performance than the one `interpret` offers.

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
