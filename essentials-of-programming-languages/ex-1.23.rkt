#lang eopl

(require "./common.rkt")

(define (list-index pred lst)
  (if (null? lst)
    #f
    (if (pred (car lst))
      0
      (let ((result (list-index pred (cdr lst))))
        (if result
          (+ 1 result)
          #f)))))

(out (list-index number? '(a 2 (1 3) b 7))
     (list-index symbol? '(a (b c) 17 foo))
     (list-index symbol? '(1 2 (a b) 3))
     (list-index number? '(a (b c) 17 foo))
     )
