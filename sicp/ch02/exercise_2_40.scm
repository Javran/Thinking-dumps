(load "../common/utils.scm")

(define (unique-pairs n)
  (concat
    (map (lambda (i)
           (map (lambda (j) (cons i j))
                (list-in-range 1 (- i 1))))
         (list-in-range 1 n))))

(out (unique-pairs 3))
; ((2 . 1) (3 . 1) (3 . 2))

(define (my-prime-sum-pairs n)
  (filter identity
          (map (lambda (p)
                 (let ((i (car p))
                       (j (cdr p)))
                   (if (prime? (+ i j))
                     (list i j (+ i j))
                     #f)))
               (unique-pairs n))))

(out (my-prime-sum-pairs 6))

(end-script)
