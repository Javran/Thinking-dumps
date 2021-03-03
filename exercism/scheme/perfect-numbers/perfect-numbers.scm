(import (rnrs))

(define (classify n)
  (if (<= n 0)
      (raise 'invalid)
      (let loop ([i 1]
                 [sum 0])
        (if (= i n)
            (cond
             [(= sum n) 'perfect]
             [(> sum n) 'abundant]
             [else 'deficient])
            (loop (+ i 1)
                  (if (= (remainder n i) 0)
                      (+ sum i)
                      sum))))))
