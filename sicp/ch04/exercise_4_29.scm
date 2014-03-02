(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; one way to implement memoize a function
;; might be make a function accept another
;; "more optimized version" of itself.
;; Suppose there is a recursive function `f`:
;;
;;   (define (f n) ... (f (- n 1)) ...)
;;
;; the problem of memoizing this function is:
;; `f` itself cannot benefit from memoization,
;; that is, `(f n)` cannot reuse the result
;; of `(f (- n 1))`, so here we make a room
;; for the "optimized version of f",
;; the new style would be:
;;
;;   (define (f f1)
;;     (lambda (n)
;;       ... (f1 (- n 1)) ...))

;; `memoize` takes a function `f`
;; that accepts another function `f1` as
;; the `optimized version` of itself.
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

;; here the observation should be that
;; the first one takes significantly
;; more time to compute the same thing.

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
