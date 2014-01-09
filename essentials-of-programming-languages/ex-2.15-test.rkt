#lang eopl

(require "./common.rkt")

(provide do-lc-exp-test)

(define (do-lc-exp-test
          var-exp
          lambda-exp
          app-exp
          ;
          var-exp?
          lambda-exp?
          app-exp?
          ;
          var-exp->var
          lambda-exp->bound-var
          lambda-exp->body
          app-exp->rator
          app-exp->rand
          )

  ; test predicates
  (let* ((v-e (var-exp 'x))
         (l-e (lambda-exp 'x v-e))
         (a-e (app-exp l-e l-e))
         (exprs (list v-e l-e a-e)))
    (assert
      (equal?
        (list #t #f #f) (map var-exp? exprs)))
    (assert
      (equal?
        (list #f #t #f) (map lambda-exp? exprs)))
    (assert
      (equal?
        (list #f #f #t) (map app-exp? exprs)))
    (out "predicates: assertion test passed.")
    'done)

  ; test extractors
  (let* (; l-e = \ x -> \y -> x y
         (l-e (lambda-exp
                'x
                (lambda-exp
                  'y
                  (app-exp
                    (var-exp 'x)
                    (var-exp 'y))))))
    (define the-x
      (lambda-exp->bound-var l-e))
    (assert (eq? the-x 'x))
    (define the-y
      (lambda-exp->bound-var
        (lambda-exp->body l-e)))
    (assert (eq? the-y 'y))
    (define the-app
      (lambda-exp->body
        (lambda-exp->body l-e)))
    (define the-app-x
      (var-exp->var
        (app-exp->rator the-app)))
    (assert (eq? the-app-x 'x))
    (define the-app-y
      (var-exp->var
        (app-exp->rand the-app)))
    (assert (eq? the-app-y 'y))
    (out "extractors: assertion test passed.")
    'done)
  ; test by defining occurs-free?
  (define (occurs-free? search-var exp)
    (cond ((var-exp? exp)
            (eq? search-var
                 (var-exp->var exp)))
          ((lambda-exp? exp)
            (and
              (not (eq? search-var
                        (lambda-exp->bound-var exp)))
              (occurs-free? search-var (lambda-exp->body exp))))
          (else
            (or (occurs-free? search-var (app-exp->rator exp))
                (occurs-free? search-var (app-exp->rand exp))))))
  (define occurs-free-test-exprs
    (list
      ; x
      (var-exp 'x)
      ; y
      (var-exp 'y)
      ; (lambda (x) (x y))
      (lambda-exp
        'x
        (app-exp
          (var-exp 'x)
          (var-exp 'y)))
      ; (lambda (y) (x y))
      (lambda-exp
        'y
        (app-exp
          (var-exp 'x)
          (var-exp 'y)))
      ; ((lambda (x) x) (x y))
      (app-exp
        (lambda-exp
          'x
          (var-exp 'x))
        (app-exp
          (var-exp 'x)
          (var-exp 'y)))
      ; (lambda (y) (lambda (z) (x (y z))))
      (lambda-exp
        'y
        (lambda-exp
          'z
          (app-exp
            (var-exp 'x)
            (app-exp
              (var-exp 'y)
              (var-exp 'z)))))))
  (assert
    (equal?
      '(#t #f #f #t #t #t)
      (map (lambda (e) (occurs-free? 'x e))
           occurs-free-test-exprs)))
  (out "occurs-free?: test passed."))
