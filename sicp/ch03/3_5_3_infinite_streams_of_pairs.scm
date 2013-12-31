(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define (prime? n)
  ; assume n >= 2
  (define (smallest-divisor n)
    (define (divides? a b)
      (= (remainder b a) 0))
    (define (square x) (* x x))
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n) ; impossible
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

; the structure of `(pairs s t)`

; <s_0,t_0> | <s_0,t_1>   <s_0,t_2> ...
; ----------+-----------+--------------
;           | <s_1,t_1> | <s_1,t_2> ...
;           +-----------+--------------
;           |           | <s_2,t_2> ...

; first piece | second piece
; ------------+-------------
;             | third  piece

; first  piece: the heads
; second piece: (head s) + (tail t)
; third  piece: recursive structure

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream
      (head s1)
      (interleave s2 (tail s1)))))

(define (pairs s t)
  (cons-stream
    (list (head s) (head t))
    (interleave
      (stream-map (lambda (x)
                    (list (head s) x))
                  (tail t))
      (pairs (tail s) (tail t)))))

(define prime-sum-pairs-stream
  (stream-filter
    (lambda (pair)
      (prime? (+ (car pair) (cadr pair))))
    (pairs integers integers)))

(print-few 10 prime-sum-pairs-stream)

(end-script)
