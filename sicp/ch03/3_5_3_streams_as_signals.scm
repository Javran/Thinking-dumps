(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define (integral integrand init-val dt)
  (define int
    (cons-stream
      init-val
      (add-streams
        (scale-stream integrand dt)
        int)))
  int)

(run-stream-test)
; todo: verify that:
; cos x = 1 - int(sin x dx)
; cos x = int( -sinx dx) + 1

(end-script)
