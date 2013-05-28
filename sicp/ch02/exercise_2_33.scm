(load "../common/utils.scm")

; the `initial` will first be combined with the last element in seq
; `accumulate` looks like `foldr` in Haskell
(define (accumulate op initial seq)
  (if (null? seq)
    ; the accumulated result goes to `initial`
    initial
    (op (car seq)
        (accumulate op initial (cdr seq)))))

(define (my-map p seq)
  (accumulate (lambda (i acc) 
                (cons (p i) acc))
              nil
              seq))

(out (my-map square (list-in-range 1 10)))
; ...

; we use `seq2` as the initial result because a list is constructed
;     by adding elements repeatly to its head
;     `seq1` can be perfectly destructed in reversed order and
;     accumulated to form the final result
(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(out (my-append (list-in-range 1 5)
                (list-in-range 6 10)))
; (1 .. 10)

; we don't care about what exactly the element is
; simply accumulate 1 to the result will do
(define (my-length seq)
  (accumulate (lambda (i acc)
                (inc acc))
              0
              seq))

(out (my-length (list-in-range 1 100)))
; 100

(end-script)
