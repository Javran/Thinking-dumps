(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

(load "exercise_5_47_eval.scm")
(load "exercise_5_47_compiler.scm")

;; leave a repl entry here for playing around
;; you can load this module and try.
(compile-and-go
 '(begin
    (define (identity x) x)
    (define (map f xs)
      (if (null? xs)
          '()
          (cons (f (car xs))
                (map f (cdr xs)))))
    ))

;; now try in your repl:
;; > (map (identity (lambda (x) (* x x))) '(1 2 3 4 5))
