#lang eopl

(require "./common.rkt")
(require "./test-utils.rkt")

(define (identifier? s)
  (and (symbol? s)
       (not (eq? s 'lambda))))

(define-datatype lc-exp lc-exp?
  (var-exp
    (var identifier?))
  (lambda-exp
    (bound-vars (list-of identifier?))
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rands (list-of lc-exp?))))

(define (parse-expression exp)
  (define (parse-variables vars)
    (if ((list-of identifier?) vars)
      vars
      (eopl:error 'parse-expression
        "invalid variable list: ~A" vars)))
  (define (parse-operands exps)
    (map parse-expression exps))
  (cond ((identifier? exp)
          (var-exp exp))
        ((or (not (list? exp))
             (null? exp))
          (eopl:error 'parse-expression
            "not a identifier or valid list: ~A" exp))
        ((eq? (car exp) 'lambda)
          ; (lambda (vars) lc-exp)
          (lambda-exp
            (parse-variables (cadr exp))
            (parse-expression (caddr exp))))
        (else
          ; application
          (app-exp
            (parse-expression (car exp))
            (parse-operands (cdr exp))))))

(define (unparse-lc-exp lexp)
  (cases lc-exp lexp
    (var-exp (var)
      var)
    (lambda-exp (vars body)
      (list 'lambda vars (unparse-lc-exp body)))
    (app-exp (rator rands)
      (map unparse-lc-exp (cons rator rands)))))

(define lc-exp1 
  (parse-expression
    '((lambda (a b c) (a b c)) (lambda (x) x) (lambda (a b) (b a)))))
(define lc-exp2
  (parse-expression
    '(lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))))

(out (unparse-lc-exp lc-exp1)
     (unparse-lc-exp lc-exp2))
