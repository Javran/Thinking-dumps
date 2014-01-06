(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./3_5_4_streams_and_delayed_evaluation_common.scm")


(define (solve-2nd dy0 y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams
                (scale-stream dy a)
                (scale-stream y b)))
  y)

(end-script)
