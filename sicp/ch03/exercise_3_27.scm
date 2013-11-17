(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./exercise_3_26_bintable.scm")

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
              (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize
    (lambda (n)
      (cond ((= n 0) 0)
            ((= n 1) 1)
            (else (+ (memo-fib (- n 1))
                     (memo-fib (- n 2))))))))

; to see the speedup ...
(out (time-test map fib      (list-in-range 1 20)))
(out (time-test map memo-fib (list-in-range 1 20)))

(end-script)
