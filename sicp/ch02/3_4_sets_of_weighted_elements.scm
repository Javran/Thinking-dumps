(load "../common/utils.scm")

; excerpted from previous code, compact version for copying
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define symbol-leaf cadr)
(define weight-leaf caddr)
(define (make-code-tree left right)
  (list left
        right
        (append (symbols left)
                (symbols right))
        (+ (weight left)
           (weight right))))
(define left-branch car)
(define right-branch cadr)
(define symbols-code-tree caddr)
(define weight-code-tree cadddr)
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (symbols-code-tree tree)))
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (weight-code-tree tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set)))
          (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair)   ; symbol
                             (cadr pair)) ; frequency
                  (make-leaf-set (cdr pairs))))))

(out (make-leaf-set 
       ; here we can passed a shuffled version of initial pair list
       (list (list 'A 4)
             (list 'C 1)
             (list 'B 2)
             (list 'D 1))))

(end-script)
