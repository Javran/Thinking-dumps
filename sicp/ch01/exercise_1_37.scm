(load "../common/utils.scm")

(define (cont-frac-rec n d k)
  (define (cont-frac-r i)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (cont-frac-r (inc i))))))
  (cont-frac-r 1))

(define (cont-frac-itr n d k)
  ; counter i from k to 1
  ; acc_next = N(i) / ( D(i) + acc ) 
  ; set acc to 0 initially
  (define (cont-frac-i i acc)
    (if (< i 1)
      acc
      (cont-frac-i (dec i) (/ (n i) (+ (d i) acc)))))
  (cont-frac-i k 0))

(define psi (/ (- (sqrt 5) 1) 2))

; need accuracy of 4 decimal places
(define tolerance 0.00001)

; (define const-1 (lambda (x) 1.0))

(let loop ((k 1))
  (let* ((result (cont-frac-itr (const 1.0) (const 1.0) k))
         (diff (abs (- result psi))))
    (display "result: ")
    (display result)
    (newline)
    (display "abs diff: ")
    (display diff)
    (newline)
    (if (>= diff tolerance)
      (loop (inc k))
      (begin
        (display "k should be at least: ")
        (display k)
        (newline)))))
; k >= 12
