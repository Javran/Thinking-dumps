; Local variables:
; proc-entry: "./my-eval.scm"
; End:

(define (my-analyze exp)
  'todo)

(define (my-eval-analyze exp env)
  ((my-analyze exp) env))
