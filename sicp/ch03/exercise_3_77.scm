(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./3_5_4_streams_and_delayed_evaluation_common.scm")

(define (integral-1 delayed-integrand init-value dt)
  (cons-stream
    init-value
    (let ((integrand (force delayed-integrand)))
      (if (stream-null? integrand)
        the-empty-stream
        (integral-1
          (delay (stream-cdr integrand))
          (+ (* dt (stream-car integrand))
             init-value)
          dt)))))

(define (solve-1 f y0 dt)
  (define y (integral-1 (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(let ((inv-precision 10000))
  (out (stream-ref (solve-1 identity 1 (/ 1.0 inv-precision)) inv-precision))
  (out (exp 1)))

(end-script)
