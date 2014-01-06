(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./3_5_4_streams_and_delayed_evaluation_common.scm")

(define (solve-2nd f dy0 y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

; re-run tests in ex 3.78
(let* ((inv-precision 10000)
       (dt (/ 1.0 inv-precision)))
  (define a 2)
  (define b -1)
  (define (f dy y)
    (+ (* a dy) (* b y)))

  (out (stream-ref
         (solve-2nd f 1 1 dt)
         inv-precision))
  (out (exp 1))
  
  (out (stream-ref
         (solve-2nd f 2 1 dt)
         inv-precision))
  (out (* 2 (exp 1)))

  (out (stream-ref
         (solve-2nd f 0 1 dt)
         (inexact->exact
           (floor (* 1.2345 inv-precision)))))
  (out (* -0.2345 (exp 1.2345))))

(end-script)
