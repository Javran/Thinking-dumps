(load "../common/utils.scm")

(define (accumulate op initial seq)
  (if (null? seq)
    ; the accumulated result goes to `initial`
    initial
    (op (car seq)
        (accumulate op initial (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    ; if the first list is empty, so will be the others
    ;     in this situation, we return nil
    nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

; both v and w are vectors
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; to understand `(map * v w)`
(let ((x (list 1 2 3))
      (y (list 8 4 7)))
  (out (map * x y)))
; ( (* 1 8) (* 2 4) (* 3 7) )
; => (8 8 21)

;        B
; AAAA   B
; AAAA x B
;  (m)   B (v)
(define (matrix-*-vector m v)
  (map (lambda (m-row)
         (dot-product m-row v))
       m))

(let ((x (list (list 1 2 3 4)
               (list 5 6 7 8)))
      (y (list 3 2 1 0)))
  (out (matrix-*-vector x y)))
; (10 34)

; transpose example:
; / A B C \     / A D \
; \ D E F /  => | B E |
;               \ C F /
; we can make (A D), (B E), (C F) lists by using `accumulate-n`
;   and then put them into a list to form the result
;   we are lucky because the result of `accumulate-n` comes naturally as a list
(define (transpose mat)
  (accumulate-n cons nil mat))

(let ((x (list (list 1 2 3 4)
               (list 5 5 6 6)
               (list 7 8 9 0))))
  (out x)
  (out (transpose x))
  (out (transpose (transpose x))))

(end-script)
