(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_4_4_common.scm")

; (and) => #t
; (and <expr>) => <expr>
; (and <expr1> ...) =>
; ((lambda (result)
;    (if result
;       (and ...)
;       #f))
;  (eval <expr1> env))
(define (eval-and exp env)
  (define (and->if exp)
    (cond ((null? exp)
            ; this is something to be evaluated
            ;   as an expr
            ;   so I quote this value
            'true-value)
          ((null? (cdr exp))
            ; last expression, simply return it
            (car exp))
          (else
            (list
              (list 'lambda (list 'result)
                    (list 'if 'result
                          (list 'and (and->if (cdr exp)))
                          'false-value))
              (list 'eval (car exp) 'env)))))
  (eval (and->if (cdr exp))
        ; extend the default environment
        ;  to include the value of `env`
        (extend-top-level-environment
          env
          '(env)
          (list env))))

; (or) => #f
; (or <expr>) => <expr>
; (or <expr1> ...) =>
; ((lambda (result)
;    (if result
;       result
;       (or ...)))
;  (eval <expr1> env))
(define (eval-or exp env)
  (define (or->if exp)
    (cond ((null? exp)
            'false-value)
          ((null? (cdr exp))
            (car exp))
          (else
            (list
              (list 'lambda (list 'result)
                    (list 'if 'result
                          'result
                          (list 'or (or->if (cdr exp)))))
              (list 'eval (car exp) 'env)))))
  (eval (or->if (cdr exp))
        (extend-top-level-environment
          env
          '(env)
          (list env))))
                       
(test-and-or eval-and eval-or)

(end-script)
