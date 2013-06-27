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

; `symbols` and `weight` will have different behavior accordingly
;   to the argument given
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (symbols-code-tree tree)))

(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (weight-code-tree tree)))

; => ((a (c d)) (b e)): 15

; let's make a tree: a:3 b:5 c:2 d:1 e:4
(let* ((ta (make-leaf 'a 3))
       (tb (make-leaf 'b 5))
       (tc (make-leaf 'c 2))
       (td (make-leaf 'd 1))
       (te (make-leaf 'e 4))
       (tree-1 (make-code-tree tc td))
       ; => a:3 b:5 (c d):3 e:4
       (tree-2 (make-code-tree ta tree-1))
       ; => (a (c d)):6 b:5 e:4
       (tree-3 (make-code-tree tb te))
       ; => (a (c d)):6 (b e):9
       (tree-4 (make-code-tree tree-2 tree-3)))
  (out tree-4))

(end-script)
