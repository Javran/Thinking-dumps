(load "../common/utils.scm")

(define (average-damping f)
  (lambda (x)
    (mid x (f x))))

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define (repeated-iter f acc counter)
    (cond ((= counter 0)
           acc)
          ((odd? counter)
           (repeated-iter f (compose f acc) (- counter 1)))
          (else ; even
            (repeated-iter (compose f f) acc (/ counter 2)))))
  (repeated-iter f identity n))
