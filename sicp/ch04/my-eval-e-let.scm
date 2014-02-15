; handle let-exp
; from ./exercise_4_6.scm

; bindings: a list of bindings,
;   each binding is a list of two elements, <var> and <exp>
; body: a sequence of one or more expressions
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (install-eval-let)
  (define (let->combination exp)
    (define (named-let? exp)
      ; the second element
      ;   should be a symbol
      ;   for named-let
      (symbol? (cadr exp)))

    (define (normal-let->combination exp)
      (define let-binding-pairs (cadr exp))
      (define let-body (cddr exp))
      (define vars (map car  let-binding-pairs))
      (define exps (map cadr let-binding-pairs))

      (cons
        ; operator
        (make-lambda vars let-body)
        ; operands
        exps))

      ; (let <proc> <bindings> <body>)
      ; =>
      ; (let ()
      ;   (define (<proc> <binding-vars>)
      ;     <body>)
      ;   (<proc> <binding-exps>))
      (define (named-let->combination exp)
        (define proc-name (cadr exp))
        (define let-binding-pairs (caddr exp))
        (define let-body (cdddr exp))
        (define vars (map car  let-binding-pairs))
        (define exps (map cadr let-binding-pairs))
        (normal-let->combination
          (list 'let '()
                (cons 'define
                      (cons (cons proc-name vars)
                            let-body))
                (cons proc-name exps))))

      (if (named-let? exp)
        (named-let->combination exp)
        (normal-let->combination exp)))

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
          ; test named let
          (mat `(let fib-iter ((a 1) (b 0) (count 10))
                  (if (= count 0)
                    b
                    (fib-iter (+ a b) a (- count 1)))) env 55)
          (mat `(let fib-iter ((a 1) (b 0) (count 14))
                  (if (= count 0)
                    b
                    (fib-iter (+ a b) a (- count 1)))) env 377)
          (mat `(let proc ((i 1) (acc 1))
                  (if (<= i 10) (proc (+ i 1) (* i acc)) acc)) env 3628800)
          (mat `(let proc ((i 1) (acc 0))
                  (if (<= i 100) (proc (+ i 1) (+ i acc)) acc)) env 5050)
          ))
      'analyze))

  (define handler
    (make-handler
      'let
      eval-let
      'todo
      test))

  (handler-register! handler)
  'ok)
