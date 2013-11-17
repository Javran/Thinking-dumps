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

(define (fib-pseudo f n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (f (- n 1))
                 (f (- n 2))))))

(define (memoize2 f)
  (let ((mem (make-table)))
    (define (memo-f n)
      (let ((cached (lookup n mem)))
        (if cached
          cached
          (let ((result (fib-pseudo memo-f n)))
            (insert! n result mem)
            result))))
    memo-f))

(define memo2-fib (memoize2 fib-pseudo))

; to see the speedup ...
(out (time-test map fib       (list-in-range 1 20)))
(out (time-test map memo-fib  (list-in-range 1 20)))
(out (time-test map memo2-fib (list-in-range 1 20)))

(define (heavy)
  (define queries (map (lambda (x) (random-range-in 1 1000))
                       (list-in-range 1 1000)))
  (time-test map memo-fib  queries)
  (time-test map memo2-fib queries))

; uncomment the following line for a heavy test
(heavy)

(end-script)
