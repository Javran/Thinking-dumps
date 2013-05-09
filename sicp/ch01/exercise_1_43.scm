(load "../common/utils.scm")

; compose-free version
(define (repeated-1 f time)
  (if (= time 0) 
    identity
    (lambda (x)
      (f ((repeated-1 f (- time 1)) x)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated-2 f time)
  (if (= time 0)
    identity
    (compose f (repeated-2 f (- time 1)))))

(out ((repeated-1 square 2) 5))
(out ((repeated-2 square 2) 5))

; use a procedure that has side-effect
(define (p-with-side-effect x)
  (out "a procedure that has side effect"))

((repeated-1 p-with-side-effect 4) 'stub)
; 4 times
(newline)

((repeated-2 p-with-side-effect 4) 'stub)
; 4 times
(newline)

; it's possible that we can achieve `repeated` in a more efficient way
;     when using the method we've seen in `fast-expt`
(define (fast-repeated f n)
  (define (repeated-iter f acc counter)
    (cond ((= counter 0)
            acc)
          ((odd? counter)
            (repeated-iter f (compose f acc) (- counter 1)))
          (else ; even
            (repeated-iter (compose f f) acc (/ counter 2)))))
  (repeated-iter f identity n))

(let ((test (lambda (repeated-impl)
              (out ((repeated-impl inc 10000) 0)))))
  (time-test test repeated-1)
  (time-test test repeated-2)
  (time-test test fast-repeated))

; TODO: we've seen that `fast-repeated` works as expected
;     I think I've been little more closer to be able to abstract `fast-expt`, `fast-mul` ... etc. as one function
