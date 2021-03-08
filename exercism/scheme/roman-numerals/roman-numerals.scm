(import (rnrs))

(define (mk-converter i v x)
  (lambda (n)
    (case n
      [(0) '()]
      [(1) (list i)]
      [(2) (list i i)]
      [(3) (list i i i)]
      [(4) (list i v)]
      [(5) (list v)]
      [(6) (list v i)]
      [(7) (list v i i)]
      [(8) (list v i i i)]
      [(9) (list i x)])))

(define one-rep (mk-converter #\I #\V #\X))
(define ten-rep (mk-converter #\X #\L #\C))
(define hundred-rep (mk-converter #\C #\D #\M))
(define thousand-rep (mk-converter #\M #\_ #\_))

(define (roman n)
  (let loop ([acc '()]
             [reps (list one-rep ten-rep hundred-rep thousand-rep)]
             [cur n])
    (if (or (zero? cur) (null? reps))
        (list->string (apply append acc))
        (let ([r (remainder cur 10)]
              [q (quotient cur 10)])
          (loop
           (cons ((car reps) r) acc)
           (cdr reps)
           q)))))
