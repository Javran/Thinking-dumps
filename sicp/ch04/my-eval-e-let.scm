; handle let-exp
; from ./exercise_4_6.scm

(define (install-eval-let)
  ; (let ((<var1> <exp1>)
  ;       (<var2> <exp2>)
  ;       ...
  ;       )
  ;   <body>)
  ; =>
  ; ((lambda (<var1> <var2> ...)
  ;    <body>)
  ;  <exp1>
  ;  <exp2>
  ;  ...
  ;  )
  (define (let->combination exp)
    (define let-binding-pairs (cadr exp))
    (define let-body (cddr exp))
    (define vars (map car  let-binding-pairs))
    (define exps (map cadr let-binding-pairs))
    (cons
      ; operator
      (make-lambda vars let-body)
      ; operands
      exps))


  (define (eval-let exp env)
    (my-eval (let->combination exp) env))

  (define (test)
    (let ((env (init-env)))
      (do-test
        eval-let
        (list
          (mat `(let ((x 1)
                      (y 2)
                      (z 3))
                  (+ x y z)) env 6)
          (mat `(let ((a 10)
                      (b 20))
                  (+ a a)
                  (* b a)) env 200)
          (mat `(let ()
                  10) env 10)
          (mat `(let ((a 10))
                  (let ((a (+ a 20)))
                    (let ((a (* a 30)))
                      (+ a a)))) env 1800)
          ))
      'ok))

  (define handler
    (make-handler
      'let
      eval-let
      test))

  (handler-register! handler)
  'ok)
