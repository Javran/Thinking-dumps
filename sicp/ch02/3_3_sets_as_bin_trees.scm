(load "../common/utils.scm")

; a set is a data structure that supports:
; union-set
; intersection-set
; element-of-set?
; adjoin-set

(define (element-of-set? x set)
  ; given that its an ordered list, we can skip some element comparisons
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
              (cons x1 (intersection-set (cdr set1)
                                         (cdr set2))))
            ((< x1 x2)
              (intersection-set (cdr set1) set2))

            ((< x2 x1)
              (intersection-set set1 (cdr set2)))))))

(let ((x1 (list-in-range 2 5))
      (x2 (list-in-range 3 7)))
  (out (element-of-set? 6 x1)
       ; #f
       (element-of-set? 6 x2)
       ; #t
       (intersection-set x1 x2)
       ; (3 4 5)
       ))

(end-script)
