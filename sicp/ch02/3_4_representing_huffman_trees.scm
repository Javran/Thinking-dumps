(load "../common/utils.scm")

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
  
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(end-script)
