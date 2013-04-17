(load "../common/utils.scm")

(define (square x) (* x x))

(define (average x y)
  (/ (+ x y) 2))

; given a guess, how to improve it?
(define (improve guess x)
  ; average of guess & (x/guess)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (my-sqrt x) (sqrt-iter 1.0 x))

(define test-sqrt
  (lambda (sqrt-to-test value)
    (let ((right-answer (sqrt value))
          (my-answer (sqrt-to-test value)))
    (out
      "built in result:"
      right-answer
      "test result:"
      my-answer
      "difference:"
      (- right-answer my-answer)))))

; an example for "good-enough?" is inadequate for small numbers
(test-sqrt my-sqrt 0.0001)
; 0.01
; 0.03 <- might considered a big error

; an example for "good-enough?" is inadequate for big numbers
; actually I don't know why it is inadequate ...
; the point might be: it's already precise enough for big numbers,
;     but the fixed precision cannot satisfiy both small numbers and big numbers
(test-sqrt my-sqrt 100000)
; 316.2
; 316.2

; I'd like to abstract the computation of Newton's method.
; My idea is:
; TODO: the code here is just a naive skeleton, need to make it work
; things we need to know is:
; how to refine?
; x[n+1] = x[n] - f(x[n])/f'(x[n]) (we need to know x[n], f, f')
; when to stop?
; (current-guess - prev-guess)/current-guess < threshold
;     (we need to know current-guess, prev-guess, threshold)
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

(define (my-sqrt-2 target)
  (do-newton-method
    1.0
    (lambda (x) (- (* x x) target))
    (lambda (x) (* 2 x))
    0.01))

; really quick for this time!
(test-sqrt my-sqrt-2 0.0001)
(test-sqrt my-sqrt-2 100000)

; yes, the end test works better for both small & big number case.
