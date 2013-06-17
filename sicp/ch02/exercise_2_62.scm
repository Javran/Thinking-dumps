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

(define (adjoin-set x set)
  (cond 
    ; empty set, insert element directly
    ((null? set) (list x))
    ; duplicate element, skip it
    ((= x (car set))
      set)
    ; element less than the first element in set
    ;   insert in front of the set
    ((< x (car set))
      (cons x set))
    (else (cons (car set)
                (adjoin-set x (cdr set))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2)
                       (cons x1 (union-set (cdr set1) set2)))
                      ((> x1 x2)
                       (cons x2 (union-set set1 (cdr set2)))))))))

(let ((x1 '(1 3 5))
      (x2 '(2 4 6 8)))
  (out (union-set x1 x2))
  )

(end-script)
