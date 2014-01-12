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
    (bound-var identifier?)
    (body lc-exp?))
  (app-exp
    (rator lc-exp?)
    (rand lc-exp?)))

(define (unparse-lc-exp-to-string exp)
  (cases lc-exp exp
    (var-exp (var)
      (symbol->string var))
    (lambda-exp (var body)
      (format "(lambda (~A) ~A)"
        var
        (unparse-lc-exp-to-string body)))
    (app-exp (rator rand)
      (format "(~A ~A)"
        (unparse-lc-exp-to-string rator)
        (unparse-lc-exp-to-string rand)))))

(define (parse-expression datum)
  (cond ((identifier? datum)
         (var-exp datum))
        ((non-empty? datum)
         (if (eqv? (car datum) 'lambda)
           (lambda-exp
             (car (cadr datum))
             (parse-expression (caddr datum)))
           (app-exp
             (parse-expression (car datum))
             (parse-expression (cadr datum)))))
        (else (report-invalid-concrete-syntax datum))))

(define (report-invalid-concrete-syntax datum)
  (eopl:error 'parse-expression
    "invalid concrete syntax: ~A" datum))

(define lc-exp1
  (parse-expression
    '((lambda (a) (a b)) c)))

(define lc-exp2
  (parse-expression
    '(lambda (x)
       (lambda (y)
         ((lambda (x)
            (x y))
         x)))))

(out (unparse-lc-exp-to-string lc-exp1)
     ; ((lambda (a) (a b)) c)
     (unparse-lc-exp-to-string lc-exp2)
     ; (lambda (x) (lambda (y) ((lambda (x) (x y)) x)))
     )
