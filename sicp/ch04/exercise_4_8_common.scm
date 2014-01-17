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

  ; (let <var> <bindings> <body>)
  ; =>
  ; (let ((y-combinator 
  ;         (lambda (f)
  ;           ((lambda (x)
  ;              (f (lambda <binding-vars>
  ;                   ((x x) <binding-vars>>))))
  ;            (lambda (x)
  ;              (f (lambda <binding-vars>
  ;                   ((x x) <binding-vars>>)))))))
  ;       (almost-recursive
  ;         (lambda (<var>)
  ;           (lambda <binding-vars>
  ;             <body>))))
  ;   ((y-combinator almost-recursive) <binding-exps>))
  (define (named-let->combination-y exp)
    (define proc-name (cadr exp))
    (define let-binding-pairs (caddr exp))
    (define let-body (cdddr exp))
    (define vars (map car  let-binding-pairs))
    (define exps (map cadr let-binding-pairs))

    ; use the argument list to generate a y-combinator
    (define (y-combinator-exp args)
      ; (lambda args (apply (x x) args))
      (define (lambda-1 args)
        (make-lambda
          args
          (list (cons '(x x) args))))

      ; (lambda (x) (f (lambda args (apply (x x) args))))
      (define (lambda-2 lambda-1)
        (make-lambda
          '(x)
          (list (list 'f lambda-1))))
      ; (lambda (f)
      ;   ((lambda (x) (f (lambda args (apply (x x) args))))
      ;    (lambda (x) (f (lambda args (apply (x x) args))))))
      (define (lambda-3 lambda-2)
        (make-lambda
          '(f)
          (list (list lambda-2 lambda-2))))
      (lambda-3 (lambda-2 (lambda-1 args))))

    ; generate the "almost" recursive function
    (define (almostr-function-exp var args body)
      (make-lambda
        (list var)
        (list (make-lambda
                args
                body))))

    (let ((var-y (gensym))
          (var-almostr (gensym)))
      ; craft a normal let:
      ; (let ((y <y-combinator>)
      ;       (almostr <almost-recursive>))
      ;   ((y almostr) <exps>))
      (define crafted-let
        (cons
          'let
          (cons
            (list (list var-y
                        (y-combinator-exp vars))
                  (list var-almostr
                        (almostr-function-exp
                          proc-name
                          vars
                          let-body)))
            (list (cons (list var-y var-almostr)
                        exps)))))
      (normal-let->combination crafted-let)))

  (if (named-let? exp)
    (named-let->combination exp)
    (normal-let->combination exp)))

(define (test-named-let let->combination)
  (define testcases
    (list
      (mat
        '(let fib-iter ((a 1)
                        (b 0)
                        (count 10))
           (if (= count 0)
             b
             (fib-iter (+ a b) a (- count 1))))
        55)
      (mat
        '(let fib-iter ((a 1)
                        (b 0)
                        (count 14))
           (if (= count 0)
             b
             (fib-iter (+ a b) a (- count 1))))
        377)
      (mat
        '(let proc ((i 1)
                    (acc 1))
           (if (<= i 10) (proc (+ i 1) (* i acc)) acc))
        3628800)
      (mat
        '(let proc ((i 1)
                    (acc 0))
           (if (<= i 100) (proc (+ i 1) (+ i acc)) acc))
        5050)))
  (define (proc exp)
    (eval (let->combination exp)
          user-initial-environment))
  (do-test proc testcases))
