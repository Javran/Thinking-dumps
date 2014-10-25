;; "-r" for the recursive version
(define (count-leaves-r tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves-r (car tree))
                 (count-leaves-r (cdr tree))))))

;; "-i" for the iterative version (with explicit counter)
(define (count-leaves-i tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((not (pair? tree)) (+ n 1))
          (else
           (count-iter
            (cdr tree)
            (count-iter (car tree) n)))))
  (count-iter tree 0))

(define (test-machine scheme-proc machine-proc)
  (for-each
   (lambda (t)
     ;; count-leaves and count-leaves-machine
     ;; should have the same output given the same input
     (assert (= (scheme-proc t)
                (machine-proc t))))
   (list
    '()
    'a
    '((a b) (c d) (e (((f))) g))
    '(1 2 3 (4 5 . 6) 7 ((8 a b d)))))
  (out "tests done"))
