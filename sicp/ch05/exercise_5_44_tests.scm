(define test-exps-ex-5-44
  '(
    ;; let's make a weird example
    (let ((f +)
          (g *)
          (+ 10)
          (* 20))
      (f + * (g + + * *)))
    ;; mixed
    (+ (* 20 10)
       (let ((+ 10))
         (* + +)))
    ;; example in book
    (begin
      ;; the detail of matrix implementation is irrelevant
      ;; so let's just keep it symbolic.
      (define (+matrix mat1 mat2)
        (list '+matrix mat1 mat2))
      (define (*matrix mat1 mat2)
        (list '*matrix mat1 mat2))
      (define (calc + * a b x y)
        (+ (* a x) (* b y)))
      (calc +matrix *matrix 'a 'b 'x 'y))
    ))

(set! test-exps
      (append test-exps test-exps-ex-5-44))
