(newline)

(define display3-1
  (lambda (arg1 arg2 arg3)
    (begin
      (display arg1)
      (display " ")
      (display arg2)
      (display " ")
      (display arg3)
      (newline))))

(define display3-2
  (lambda (arg1 arg2 arg3)
    (display arg1)
    (display " ")
    (display arg2)
    (display " ")
    (display arg3)
    (newline)))

(display3-1 1 2 3)
(display3-2 1 2 3)
