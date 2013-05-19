(load "../common/utils.scm")

(load "./1_4_interval_arithmetic.scm")

(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)

; pretty-print
(define (print-interval a)
  (display "[")
  (display (lower-bound a))
  (display "-")
  (display (upper-bound a))
  (display "]")
  (newline))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center x)
  (average (lower-bound x)
           (upper-bound x)))

(define (width x)
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

(define (percent x)
  (/ (width x)
     (center x)))

(define (make-center-percent c p)
  (make-center-width c (* c p)))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; Demonstrate that Lem is right
; I'll simply run some test case
(let ((r1 (make-center-width 10 0.1)) ; 10+-0.5
      (r2 (make-center-width 15 0.2)) ; 15+-0.2
      )
  (print-interval r1)
  (print-interval r2)
  (print-interval (par1 r1 r2))
  (print-interval (par2 r1 r2))
  ; par1 & par2 produce different result depsite coming from the same formula
  )

; Investigate the behavior of the system
;     on a variety of arithmetic expressions.
(let ((a (make-center-width 20 1))
      (b (make-center-width 30 2))
      (c (make-center-width 40 3))
      (one (make-center-width 1 0)))
  ; (b - a) / c = b/c - a/c
  (print-interval (div-interval (sub-interval b a)
                                c))
  (print-interval (sub-interval (div-interval b c)
                                (div-interval a c))))
; different

; Make some intervals A and B, and use them in computing the expressions A/A and A/B.
(let ((a (make-center-percent 100 0.01))
      (b (make-center-percent 200 0.05)))
  (let ((a/a (div-interval a a))
        (a/b (div-interval a b)))
    (out "center:"
         (center a/a)  ; ~= 1
         (center a/b)) ; ~= 0.5
    (out "percent:"
         (percent a/a)  ; 0.02?
         (percent a/b)) ; 0.06?
    ; TODO: I have no idea what the result is supposed to be ...
    (print-interval a/a)
    (print-interval a/b)))
