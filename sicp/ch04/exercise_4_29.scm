(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (memoize f)
  (let ((retval-alist nil))
    (define (memoized-f . args)
      (cond ((assoc args retval-alist) =>
             cadr)
            (else
             (let ((result (apply (f memoized-f) args)))
               (set! retval-alist
                     (cons (list args result)
                           retval-alist))
               result))))
    memoized-f))
             


;; Exhibit a program that runs more slowly
;; without memoization
(define (fib-aux fib)
  (lambda (n)
    (if (<= n 1)
        n
        (+ (fib (- n 1))
           (fib (- n 2))))))

(define (fib n)
  ((fib-aux fib) n))

(define (fib-1 n)
  ((memoize fib-aux) n))

(time-test fib 25)
(time-test fib-1 25)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
