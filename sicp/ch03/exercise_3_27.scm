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
; (heavy)

#|
(memo-fib 3)
; first calculate `memo-fib`
(memoize f)
; create new env E1(parent=G), f = (lambda (n) ...) 
(let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result
              (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result)))))
; create new env E2(parent=E1), table = <impl hidden>
(lambda (x)
  (let ((previously-computed-result
          (lookup x table)))
    (or previously-computed-result
        (let ((result (f x)))
          (insert! x result table)
          result))))
; `memo-fib` evaluated
; in global env G, bind `memo-fib` with this lambda
; create new env E3(parent=E2), x=3
(let ((previously-computed-result
        (lookup x table)))
  (or previously-computed-result
      (let ((result (f x)))
        (insert! x result table)
        result)))
; create new env E4(parent=E3), previously-computed-result= #f (not stored yet)
(or previously-computed-result
    (let ((result (f x)))
      (insert! x result table)
      result))
; keep going
(let ((result (f x)))
  (insert! x result table)
  result)
; we need to evaluate (f x)
; create new env E5(parent=E4), n = 3
(cond ((= n 0) 0)
      ((= n 1) 1)
      (else (+ (memo-fib (- n 1))
               (memo-fib (- n 2)))))
; we need to evaluate (memo-fib 2)
; `memo-fib` lies in G
; create new env E6(parent=E2), x = 2
(let ((previously-computed-result
        (lookup x table)))
  (or previously-computed-result
      (let ((result (f x)))
        (insert! x result table)
        result)))
; create new env E7(parent=E6), previously-computed-result = #f (not stored yet)
(or previously-computed-result
    (let ((result (f x)))
      (insert! x result table)
      result))
; we need to evaluate (f x)
; create new env E8(parent=E7), n = 2
(cond ((= n 0) 0)
      ((= n 1) 1)
      (else (+ (memo-fib (- n 1))
               (memo-fib (- n 2)))))
; too verbose ... I'll omit all envs from now
; x = 1, (memo-fib 1)
; => previously-computed-result = #f
; => n = 1, (lambda (n) ...) => 1
; => (insert! x result table), x = 1, result = 1, table from E2
; => result (1)
; evaluate (memo-fib 0)
; ...
; => (insert! x result table), x = 0, result = 0, table from E2
; back to (+ (memo-fib 1) (memo-fib 0))
; => (insert! x result table), x = 2, result = 1, table from E2
; back to (+ (memo-fib 2) (memo-fib 1))
; => (+ 1 (memo-fib 1))
; (memo-fib 1)
; => 1 (result cached in table)
; => (+ 1 1)
; => (insert! x result table), x = 3, result = 2, table from E2
; result = 2

|#

(end-script)
