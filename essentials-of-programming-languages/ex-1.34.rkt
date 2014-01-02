#lang eopl

(require "./common.rkt")

; path :: (Int, BST) -> [Path]
; usage: return the path of finding `n` in `bst`
;   the path is represented as a list of `left` or `right`
(define (path n bst)
  (define lson cadr)
  (define rson caddr)
  (cond ((null? bst)
          (eopl:error 'path
            "Undefined case~%"))
        ((= n (car bst))
          '())
        ((< n (car bst))
          (cons 'left (path n (lson bst))))
        ((> n (car bst))
          (cons 'right (path n (rson bst))))))

(let ((tree '(14 ( 7 ()
                     (12 () ()))
                 (26 (20 (17 () ())
                         ())
                     (31 () ())))))
  (for-each
    (lambda (x)
      (display x) (out ":")
      (out (path x tree)))
    '(14 7 12 26 20 17 31)))
