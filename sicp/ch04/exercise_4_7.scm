(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_4_5_common.scm")
(load "./exercise_4_6_common.scm")

; bindings: a list of bindings,
;   each binding is a list of two elements, <var> and <exp>
; body: a sequence of one or more expressions
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

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

(define (let*? exp)
  (tagged-list? 'let* exp))

; yes it is sufficient to write the action like this
;   one concern might be that the environment has not yet
;   been applied with new bindings, but as the evaluation process
;   being carried on, the `eval-let*` eventually will call another `eval`
;   with an environment in which the variables are evaluated and bound
(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))

(out
  (eval-let*
    (let*->nested-lets
      '(let* ((x 1)
              (y (+ 1 x))
              (z (* 2 y)))
         (display x) (newline)
         (display y) (newline)
         (display z) (newline)
         (+ x y z)))
    (the-environment)))
; 1,2,4 (outputed by evaluation)
; 7     (outputed by `out`)

(newline)
; now the example from book
(out
  (eval-let*
    '(let* ((x 3)
            (y (+ x 2))
            (z (+ x y 5)))
      (* x z))
    (the-environment)))
; 39

(end-script)
