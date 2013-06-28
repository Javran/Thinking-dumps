(load "../common/utils.scm")

; my solution can be broken down into 2 parts:
; * the implementation of ordered set
;   exactly what we've done before
; * the implementation of successive merge process
;   given that the set is sorted (ordered) by its frequency,
;   we simply take and remove the first two elements from list,
;   and insert the resulting tree into the set again until there's only one tree remaining

; ---- code excerpted from ex 2.62
; procedures implemented:
; * union-set
; * intersection-set
; * element-of-set?
; * adjoin-set
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond ((= x1 x2)
              (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2)
              (intersection-set (cdr set1) set2))
            ((< x2 x1)
              (intersection-set set1 (cdr set2)))))))
(define (adjoin-set x set)
  (cond 
    ((null? set) (list x))
    ((= x (car set)) set)
    ((< x (car set)) (cons x set))
    (else (cons (car set) (adjoin-set x (cdr set))))))
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                      ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                      ((> x1 x2) (cons x2 (union-set set1 (cdr set2)))))))))

(end-script)
