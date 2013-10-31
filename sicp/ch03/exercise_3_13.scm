(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z (make-cycle (list 'a 'b 'c)))

; I guess this will run into an infinite list
; define an auxiliary function to guaranteed that
; this code can be terminated.


(define (take n ls)
  (if (or (<= n 0) (null? ls))
    nil
    (cons (car ls) (take (- n 1) (cdr ls)))))

(define z (make-cycle (list 'a 'b 'c)))

(out (take 100 z))

(end-script)
