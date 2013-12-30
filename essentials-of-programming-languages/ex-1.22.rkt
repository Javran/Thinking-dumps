#lang eopl

(require "./common.rkt")

; filter-in :: ((a -> Bool), [a]) -> [a]
; usage: return only elements from `lst` that
;   satisfies `pred` 
(define (filter-in pred lst)
  (if (null? lst)
    '()
    (let ((rest-results (filter-in pred (cdr lst))))
      (if (pred (car lst))
        (cons (car lst) rest-results)
        rest-results))))

(out (filter-in number? '(a 2 (1 3) b 7))
     (filter-in symbol? '(a (b c) 17 foo))
     (filter-in number? '(a (b c) 17 foo))
     )
