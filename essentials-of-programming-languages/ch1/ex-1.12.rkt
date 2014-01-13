#lang eopl

(require "../common.rkt")

(define (subst new old slist)
  (if (null? slist)
    '()
    (cons
      #| inline code of
      (subst-in-s-exp new old (car slist))
      |#
      (let ((sexp (car slist)))
        (if (symbol? sexp)
          (if (eqv? sexp old)
            new
            sexp)
          (subst new old sexp)))
      (subst new old (cdr slist)))))

#|
(define (subst-in-s-exp new old sexp)
  (if (symbol? sexp)
    (if (eqv? sexp old)
      new
      sexp)
    (subst new old sexp)))
|#

(out (subst 'a 'b '((b c) (b () d))))
