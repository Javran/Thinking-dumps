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

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define tree-123 (make-tree 2 (make-tree 1 nil nil)
                              (make-tree 3 nil nil)))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree)
                                        result-list)))))
  (copy-to-list tree nil))

(out (tree->list-1 tree-123))
(out (tree->list-2 tree-123))
; (1 2 3)

; TODO: to be verified
; a. Do the two procedures produce the same result for every tree? 
;   They should be the same as they have similiar structure
;   to convert trees to lists ({merge} {left} {entry} {right}) 
; b. Do the two procedures have the same order of growth in the
;   number of steps required to convert a balanced tree with n ele-
;   ments to a list?
;   `tree->list-2` might have slightly more steps to convert a tree
;   because it is a tail-recursive procedure and iterative
;   but they have same order of growth in terms of number of steps.

(end-script)
