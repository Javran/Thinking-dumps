(load "../common/utils.scm")

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (non-empty? tree))
          (if (odd? tree)
            (square tree)
            0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))


(define tree-x
  (list (list 1 2 3)
        (list 4 5 6)
        (list 7 8 9)))

(out (sum-odd-squares tree-x)
     (apply + (map square (filter odd? (list-in-range 1 9)))))
; two results should be the same: 165

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 ; fib 1 = 1
            0 ; fib 0 = 0
            n))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
      nil
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))
  (next 0))

(out (even-fibs 10)
     (filter even? (map fib (list-in-range 0 10))))
; two results should be the same:
; (0 2 8 34)

(end-script)
