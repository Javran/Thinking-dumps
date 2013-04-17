(load "../common/utils.scm")

; this procedure is copied from exercise 1.7 to make the cube root function conveniently
(define do-newton-method
  (lambda (init-guess
           target-f
           target-f-diff
           good-threshold)
    (let loop ((prev-guess #f)
               (current-guess init-guess))
      (let ((next-guess-lazy (lambda ()
                               (- current-guess (/ (target-f current-guess)
                                                   (target-f-diff current-guess))))))
        (if prev-guess
          ; we have a prev-guess, test if it's good enough 
          (if (< (abs (/ (- current-guess prev-guess) current-guess))
                 good-threshold)
            ; good enough!
            current-guess
            ; else
            (loop current-guess (next-guess-lazy)))
          ; we don't have a prev-guess
          (loop current-guess (next-guess-lazy)))))))

(define (cube-root-built-in x)
  ; define the cube root by built-in function `exp` and `log`
  ; note this function will fail when x <= 0
  (exp (* (/ 1 3) (log x))))

(define (my-cube-root target)
  (do-newton-method
    1.0
    (lambda (x) (- (* x x x) target))
    (lambda (x) (* 3 x x))
    0.01))

(define test-cube-root
  (lambda (func-to-test value)
    (let ((right-answer (cube-root-built-in value))
          (my-answer (my-cube-root value)))
    (out
      "built in result:"
      right-answer
      "test result:"
      my-answer
      "difference:"
      (- right-answer my-answer)))))

(test-cube-root my-cube-root 123456)
; ~ 49.80
(test-cube-root my-cube-root -27)
; note 'cube-root-built-in' does not handle negative argument correctly
; actually '-3' should be the exact answer
