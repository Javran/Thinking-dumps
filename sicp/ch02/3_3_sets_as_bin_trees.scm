(load "../common/utils.scm")

; a set is a data structure that supports:
; union-set
; intersection-set
; element-of-set?
; adjoin-set

; tree definition and corresponding accessors
(define (make-tree entry left right)
  (list entry left right))

(define entry car)
(define left-branch cadr)
(define right-branch caddr)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
          (element-of-set? x (left-branch set)))
        ((> x (entry set))
          (element-of-set? x (right-branch set)))))

(let ((set (make-tree 2
                    (make-tree 1 nil nil)
                    (make-tree 3 nil nil))))
  (out (map
         (lambda (x)
           (element-of-set? x set))
         (list-in-range 1 4))))
  ; (#t #t #t #f)
(newline)

(define (adjoin-set x set)
  (cond ((null? set)
          (make-tree x nil nil))
        ((= x (entry set))
          set)
        ((< x (entry set))
          (make-tree (entry set)
                     (adjoin-set x (left-branch set))
                     (right-branch set)))
        ((> x (entry set))
          (make-tree (entry set)
                     (left-branch set)
                     (adjoin-set x (right-branch set))))))

; make a full bin tree in range 3-9
(let ((s (fold-right adjoin-set nil (reverse '(6 4 8 3 7 5 9)))))
  (out s)
  (out (map
         (lambda (x) (element-of-set? x s))
         (list-in-range 1 10))))
  ; #f for first two and the last element

; TODO: impl B-trees / red-black trees if possible?

(end-script)
