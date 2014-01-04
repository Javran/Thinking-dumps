(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define (RC R C dt)
  (define (RC-voltage i v0)
    (add-streams
      ; Ri
      (scale-stream i R)
      ; v0 + 1/c * integral
      (integral
        (scale-stream i (/ 1 c))
        v0
        dt)))
  RC-voltage)

; .. how can I verify the correctness?

(end-script)
