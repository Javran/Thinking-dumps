(import (rnrs))

(define (classify n)
  (let loop ([i 2]
             [sum 1])
    (if (= i n)
        (cond
         [(= sum n) 'perfect]
         [(> sum n) 'abundant]
         [else 'deficient])
        (loop (+ i 1)
              (if (= (remainder n i) 0)
                  (+ sum i)
                  sum)))))
