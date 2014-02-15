; handle let*-exp
; from ./exercise_4_7.scm

(define (install-eval-let*)
  ; (let* ((<var1> <exp1>)
  ;        (<var2> <exp2>)
  ;        ...
  ;       )
  ;   <body>)
  ; =>
  ; (let ((<var1> <exp1>))
  ;   (let ((<var2> <exp2>))
  ;     ...
  ;     <body>
  ;     ... ; parentheses
  ;   ))
  (define (let*->nested-lets exp)
    ; consume bindings to make nested let
    (define (consume-bindings vars exps body)
      (if (null? vars)
        ; all pairs are consumed
        ;   just leave an empty binding,
        ;   we cannot expose `body` directly
        ;   because the body is a seq of expressions
        ;   rather than exactly one expression
        (make-let '() body)
        (make-let
          (list (list (car vars) (car exps)))
          ; now remember that this body part
          ;   has to be a list of sequences.
          ; since `let` is just a single expression
          ;   we need to wrap it inside a list
          (list
            (consume-bindings
              (cdr vars)
              (cdr exps)
              body)))))

    (define let*-binding-pairs (cadr exp))
    (define let*-body (cddr exp))
    (consume-bindings
      (map car  let*-binding-pairs)
      (map cadr let*-binding-pairs)
      let*-body))

  (define (eval-let* exp env)
    (my-eval (let*->nested-lets exp) env))

  (define (test)
    (let ((env (init-env)))
      (do-test
        eval-let*
        (list
          (mat `(let* ((x 1)
                       (y (+ 1 x))
                       (z (* 2 y)))
                  (+ x y z)) env 7)
          (mat `(let* ((x 3)
                       (y (+ x 2))    ; y = 5
                       (z (+ x y 5))) ; z = 3 + 5 + 5
                  (* x z)) env 39)))
      'analyze))

  (define handler
    (make-handler
      'let*
      eval-let*
      'todo
      test))

  (handler-register! handler)
  'ok)
