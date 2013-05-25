(load "../common/utils.scm")

; mobile: left-branch & right-branch
(define (make-mobile left right)
  (list left right))

; branch: length + (weight or mobile)
(define (make-branch len structure)
  (list len structure))

; task a, selectors
(define left-branch car)
(define right-branch cadr)

(define branch-length car)
(define branch-structure cadr)

(define test-data-1
  (make-mobile (make-branch 10 1)
               (make-branch 5 2)))

(out (branch-length (left-branch test-data-1))
     ; 10
     (branch-length (right-branch test-data-1))
     ; 5
     (branch-structure (left-branch test-data-1))
     ; 1
     (branch-structure (right-branch test-data-1))
     ; 2
     )

(end-script)
