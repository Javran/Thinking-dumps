(load "../common/utils.scm")

; recursive process
(define (f-recur n)
  (if (< n 3)
    n
    (+      (f-recur (- n 1))
       (* 2 (f-recur (- n 2)))
       (* 3 (f-recur (- n 3))))))

; iterative process
(define (f-iter n)
  (define (f-iter-with-prev-3
            count
            f-min-1
            f-min-2
            f-min-3)
    (let ((next-f
            (+      f-min-1
               (* 2 f-min-2)
               (* 3 f-min-3))))
      (if (= count 0)
        ; task done, return next-f
        next-f
        (f-iter-with-prev-3
          (- count 1)
          next-f
          f-min-1
          f-min-2))))
  (if (< n 3)
    n
    (f-iter-with-prev-3 (- n 3) 2 1 0)))

; equality test
(out "Column: input, recursive output, iterative output")
(let loop ((i 0))
  (if (< i 10)
    (begin
      (display i)
      (display ":\t")
      (display (f-recur i))
      (display ",\t")
      (display (f-iter i))
      (newline)
      (loop (+ i 1)))))
