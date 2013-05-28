(load "../common/utils.scm")

(define (my-map f ls)
  (if (null? ls)
    nil
    (cons (f (car ls))
          (my-map f (cdr ls)))))

(out (my-map square (list-in-range 1 5)))
; 1 4 9 16 25

(define (my-filter pred ls)
  (if (null? ls)
    nil
    (let ((rest (my-filter pred (cdr ls))))
      (if (pred (car ls))
        (cons (car ls) rest)
        rest))))

(out (my-filter odd? (list-in-range 1 10)))
; 1 3 5 7 9

(define (accumulate op initial seq)
  (if (null? seq)
    initial
    (op (car seq)
        (accumulate op initial (cdr seq)))))

(out (accumulate + 0 (list-in-range 1 5)))
; 15
(out (accumulate * 1 (list-in-range 1 5)))
; 120
(out (accumulate cons nil (list 1 2 3 4 5)))
; (1 2 3 4 5)

(define enumerate-interval list-in-range)

(out (enumerate-interval 2 7))
; (2 3 4 5 6 7)

; actually it's the process of `flatten` a tree,
;     and is identical to `fringe`
(define (enumerate-tree tree)
  (cond ((null? tree)
          nil)
        ((not (non-empty? tree))
          (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(out (enumerate-tree (list 1 (list 2 (list 3 4) nil nil 5))))
; (1 2 3 4 5)

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map
                square
                (filter
                  odd?
                  (enumerate-tree tree)))))

(out (sum-odd-squares (list 1 2 3 (list 4 5)))
     (apply + (map square '(1 3 5))))
; 35

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 ; fib 1 = 1
            0 ; fib 0 = 0
            n))

(define (even-fibs n)
  ; not necessary to call `accumulate` at all...
  (filter 
    even?
    (map fib
         (enumerate-interval 0 n))))

(out (even-fibs 10)
     (filter even? (map fib (list-in-range 0 10))))
; (0 2 8 34)

(define (list-fib-squares n)
  (map square (map fib (list-in-range 0 n))))

(out (list-fib-squares 10))
; ...

(define (product-of-squares-of-odd-elements seq)
  (accumulate * 1 (map square (filter odd? seq))))

(out (product-of-squares-of-odd-elements (list-in-range 1 5)))

(end-script)
