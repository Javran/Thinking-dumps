(import (rnrs))

(define (sieve n)
  (if (< n 2)
      '()
      (let ([vec (make-vector (+ n 1) #t)])
        ;; set #0 and #1 for correctness of vec, this is otherwise unnecessary.
        (vector-set! vec 0 #f)
        (vector-set! vec 1 #f)
        (let ([fill-forward
               (lambda (x)
                 ;; sets multiples of x (excluding x itself) to #f
                 (let loop ([i (+ x x)])
                   (and
                    (<= i n)
                    (begin
                      (vector-set! vec i #f)
                      (loop (+ i x))))))])
          (let loop ([i 2]
                     [rev-primes '()])
            (if (> i n)
                (reverse! rev-primes)
                (if (vector-ref vec i)
                    (begin
                      (fill-forward i)
                      (loop (+ i 1) (cons i rev-primes)))
                    (loop (+ i 1) rev-primes))))))))

