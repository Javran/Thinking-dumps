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

(define (total-weight mobile)
  (define (total-weight-branch branch)
    (let ((struct (branch-structure branch)))
      (if (number? struct)
        struct
        (total-weight struct))))

  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (+ (total-weight-branch left)
       (total-weight-branch right))))

(out (total-weight test-data-1))
; 3 (1+2)

(define test-data-2
  (make-mobile (make-branch 10 test-data-1)
               (make-branch 3 (make-mobile
                                (make-branch 4 16)
                                (make-branch 1 64)))))

(out (total-weight test-data-2))
; 83 (3+16+64)

(end-script)
