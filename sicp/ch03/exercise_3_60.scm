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

(load "./exercise_3_59_common.scm")

(define result
  (add-streams
    (mul-series sine-series sine-series)
    (mul-series cosine-series cosine-series)))

(out
  "result:"
  (exact->inexact (series-sum 10 result)))

(define (powers x)
  (define powers-aux
    (cons-stream
      1
      (scale-stream powers-aux x)))
  powers-aux)

(end-script)
