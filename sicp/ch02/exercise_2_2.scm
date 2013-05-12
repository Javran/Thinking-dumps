(load "../common/utils.scm")

(define make-point  cons)
(define x-point     car)
(define y-point     cdr)

(define make-segment    cons)
(define start-segment   car)
(define end-segment     cdr)

(define (midpoint-segment seg)
  (let ((a (start-segment seg))
        (b (end-segment seg)))
    (make-point (average (x-point a) (x-point b))
                (average (y-point a) (y-point b)))))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (print-segment s)
  (display "[")
  (print-point (start-segment s))
  (display "-")
  (print-point (end-segment s))
  (display "]"))

(define p1 (make-point 1 2))
(define p2 (make-point 3 4))

(print-point p1)
(newline)
; (1,2)

(print-point p2)
(newline)
; (3,4)

(define seg (make-segment p1 p2))

(print-segment seg)
(newline)
; [(1,2)-(3,4)]

(print-point (midpoint-segment seg))
(newline)
; (2,3)
