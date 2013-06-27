(load "../common/utils.scm")

(define (make-leaf symbol weight)
  ; * signature `leaf` indicating data type 
  ; * the symbol of the leaf
  ; * weight
  (list 'leaf symbol weight))

; the first element of the list is used as a hint of data structure
(define (leaf? object)
  (eq? (car object) 'leaf))
  
; getters 
(define symbol-leaf cadr)
(define weight-leaf caddr)


(end-script)
