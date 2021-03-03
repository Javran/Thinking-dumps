(import (rnrs))

(define (knapsack capacity weights values)
  (let ([ws (list->vector weights)]
        [vs (list->vector values)])
    (let ([len (vector-length ws)])
      (let loopi ([i 0]
                  [v-prev (make-vector (+ capacity 1) 0)]
                  [v-cur (make-vector (+ capacity 1))])
        ;; current iteration computes v-cur,
        ;; in which v-cur[w] is the best value possible using items from 0-th to i-th (inclusive)
        ;; and v-prev[w] is the best value possible using items from 0-th to (i-1)-th (inclusive)
        (if (= i len)
            ;; done.
            (if (= len 0)
                0
                ;; find max value from v-prev
                (let next ([i 1]
                           [cur-max (vector-ref v-prev 0)])
                  (if (> i capacity)
                      cur-max
                      (next (+ i 1) (max cur-max (vector-ref v-prev i))))))
            (let ([item-weight (vector-ref ws i)]
                  [item-value (vector-ref vs i)])
              (let loopw ([w 0])
                (if (> w capacity)
                    ;; next iteration, rotate two vector.
                    (loopi (+ i 1) v-cur v-prev)
                    (begin
                      (vector-set! v-cur w
                                   (if (> item-weight w)
                                       (vector-ref v-prev w)
                                       (max
                                        (vector-ref v-prev w)
                                        (+ (vector-ref v-prev (- w item-weight)) item-value))))
                      (loopw (+ w 1)))))))))))

(display
 (knapsack 10 '(2 2 2 2 10) '(5 5 5 5 21)))
(display
 (knapsack 100 '() '()))
