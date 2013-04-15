(load "../common/utils.scm")

(define sum-squ-of-larger-two
  (lambda (a b c)
    (let ((sorted (sort (list a b c) >)))
      (let ((x (car sorted))
            (y (cadr sorted))
            (square (lambda (x) (* x x))))
        (+ (square x) (square y))))))

(out 
  (sum-squ-of-larger-two 2 3 4)
  ; 9 + 16 -> 25
  (sum-squ-of-larger-two 3 2 4)
  ; 25
  (sum-squ-of-larger-two 4 2 3)
  ; 25
  (sum-squ-of-larger-two 4 3 2)
  ; 25
  )
