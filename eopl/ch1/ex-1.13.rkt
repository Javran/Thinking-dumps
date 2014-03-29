#lang eopl

(require "../common.rkt")

(define (subst new old slist)
  (map
    (lambda (sexp)
      (if (symbol? sexp)
        (if (eqv? sexp old)
          new
          sexp)
        (subst new old sexp)))
    slist))

(out (subst 'a 'b '((b c) (b () d))))
