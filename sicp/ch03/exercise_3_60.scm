(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

; see "./exercise_3_60.pdf" for demonstration

(define (mul-series s1 s2)
  (let ((ah (stream-car s1))
        (at (stream-cdr s1))
        (bh (stream-car s2))
        (bt (stream-cdr s2)))
    (cons-stream
      (* ah bh)
      (add-streams
        (add-streams
          (scale-stream at bh)
          (scale-stream bt ah))
        (cons-stream
          0
          (mul-series at bt))))))

(end-script)
