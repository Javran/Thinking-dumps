(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./3_5_4_streams_and_delayed_evaluation_common.scm")

; let a^2 + 4b = 0
; => a = 2, b = -1

(define a 2)
(define b -1)

(define (solve-2nd dy0 y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams
                (scale-stream dy a)
                (scale-stream y b)))
  y)

;  y = c1 * e^t + c2 * t * e^t
; dy = c1 * e^t + c2 * ( e^t + t * e^t )
;    = (c1 + c2) * e^t + c2 * t * e^t

; let y(0) = 1, dy(0) = 1
;  y(0) = c1      = 1
; dy(0) = c1 + c2 = 1
; => c1 = 1, c2 = 0
;
; =>  y = e^t
; => dy = e^t
; =>  y(1) = e
(let ((inv-precision 10000))
  (out (stream-ref (solve-2nd 1 1 (/ 1.0 inv-precision)) inv-precision))
  (out (exp 1)))

; let y(0) = 1, dy(0) = 2
;  y(0) = c1      = 1
; dy(0) = c1 + c2 = 2
; => c1 = 1, c2 = 1
;
; =>  y =   e^t + t * e^t
; => dy = 2 e^t + t * e^t
; =>  y(1) = 2e
(let ((inv-precision 10000))
  (out (stream-ref (solve-2nd 2 1 (/ 1.0 inv-precision)) inv-precision))
  (out (* 2 (exp 1))))

; let y(0) = 1, dy(0) = 0
;  y(0) = c1      = 1
; dy(0) = c1 + c2 = 0
; => c1 = 1, c2 = -1
;
; =>  y = e^t - t * e^t
; => dy =     - t * e^t
; =>  y(1.2345) = e^1.2345 - 1.2345 * e^1.2345
; =>            = -0.2345 * e^1.2345
(let ((inv-precision 10000))
  (out (stream-ref (solve-2nd 0 1 0.0001) 12345))
  (out (* -0.2345 (exp 1.2345))))

(end-script)
