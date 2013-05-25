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

(define (branch-struct-safe-apply branch f g)
  ; apply `f` on branch-structure if it is a mobile
  ;   elsewise `g` is applied (in this case the structure is a number)
  (let ((struct (branch-structure branch)))
    (if (number? struct)
      (g struct)
      (f struct))))

(define (branch-total-weight branch)
  (branch-struct-safe-apply
    branch
    mobile-total-weight ; mobile-case
    identity)) ; number-case

(define (mobile-total-weight mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (+ (branch-total-weight left)
       (branch-total-weight right))))

(out (mobile-total-weight test-data-1))
; 3 (1+2)

(define test-data-2
  (make-mobile (make-branch 10 test-data-1)
               (make-branch 3 (make-mobile
                                (make-branch 4 16)
                                (make-branch 1 64)))))

(out (mobile-total-weight test-data-2))
; 83 (3+16+64)

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-total-weight branch)))

(define (mobile-balanced? mobile)
  ; if a mobile is balanced
  ;   all of the sub-mobiles are required to be balanced
  (if (and (branch-struct-safe-apply
             (left-branch mobile) ; on left branch
             mobile-balanced? ; for mobile, check if it is balanced
             (const #t)) ; for numbers, always true
           (branch-struct-safe-apply
             (right-branch mobile) ; on right branch
             mobile-balanced?
             (const #t)))
    ; go on
    (= (branch-torque (left-branch mobile))
       (branch-torque (right-branch mobile)))

    ; else
    #f))

(out (mobile-balanced? test-data-1))
; #t because: 10*1 = 5*2
(out (mobile-balanced? test-data-2))
; #f because: 10*3 != 3*(16+64)

(define test-data-3
  (make-mobile (make-branch 80 test-data-1)
               (make-branch 3 (make-mobile
                                (make-branch 4 16)
                                (make-branch 1 64)))))

(out (mobile-balanced? test-data-3))
; #t because: 10*1 = 5*2 and 4*16 = 1*64 and 80*(1+2) = 3*(16+64)

(end-script)
