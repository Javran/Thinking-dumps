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



(end-script)
