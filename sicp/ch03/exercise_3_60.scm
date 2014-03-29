(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./exercise_3_59_common.scm")
(load "./exercise_3_60_common.scm")

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

; test sin x * cos x = sin 2x / 2
(out (exact->inexact
       (series-sum 10 (mul-series sine-series cosine-series))))
(out (/ (sin 2) 2))

(end-script)
