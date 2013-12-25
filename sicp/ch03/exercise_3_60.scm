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

(define (powers x)
  (define powers-aux
    (cons-stream
      1
      (scale-stream powers-aux x)))
  powers-aux)

; verify that our estimation is correct
(out (exact->inexact 
       (series-sum
         10 
         ((zip-streams-with *) (powers 1.1) sine-series))))
(out (sin 1.1))

(out (exact->inexact 
       (series-sum
         10 
         ((zip-streams-with *) (powers 1.1) cosine-series))))
(out (cos 1.1))
(newline)

(define (result-x x)
  (let ((  sine-series-x ((zip-streams-with *)
                           (powers x)
                             sine-series))
        (cosine-series-x ((zip-streams-with *)
                           (powers x)
                           cosine-series)))
    (add-streams
      (mul-series   sine-series-x   sine-series-x)
      (mul-series cosine-series-x cosine-series-x))))

(define (test x)
  (format #t
          "result (x=~A):~%~A~%"
          x
          (exact->inexact
            (series-sum 10 (result-x x)))))

(test 1)
(test 0.1)
(test 1.1)

(end-script)
