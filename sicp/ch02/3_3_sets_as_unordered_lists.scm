(load "../common/utils.scm")

; a set is a data structure that supports:
; union-set
; intersection-set
; element-of-set?
; adjoin-set

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
          (cons (car set1)
                (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(let ((set1 (list-in-range 1 5))
      (set2 (list-in-range 3 7)))
  (out (map (lambda (x) (element-of-set? x set1)) (list-in-range 1 10)))
  ; #t 5 times, #f 5 times
  (out (adjoin-set 2 set2))
  ; 4 added
  (out (adjoin-set 3 set2))
  ; unchanged
  (out (intersection-set set1 set2))
  ; (3 4 5) in common
  )

(end-script)
