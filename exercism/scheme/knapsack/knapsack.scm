(import (rnrs))

(define (knapsack capacity weights values)
  (let ([ws (list->vector weights)]
        [vs (list->vector values)])
    (let* ([len (vector-length ws)]
           [memo
            (let ([v (make-vector len)])
              (let fill ([i 0])
               (if (< i len)
                   (begin
                     (vector-set! v i (make-vector (+ capacity 1) #f))
                     (fill (+ i 1)))
                   v)))])
      (define (best i weight)
        ;; (best i weight) searches from 0th to i-th item and return the maximum value
        ;; with an exact weight.
        (if (or (< i 0) (<= weight 0))
            0
            (let ([cached (vector-ref (vector-ref memo i) weight)])
              (or cached
                  (let* ([item-weight (vector-ref ws i)]
                         [item-value (vector-ref vs i)]
                         [result
                          (if (> item-weight weight)
                              (best (- i 1) weight)
                              (max
                               (best (- i 1) weight)
                               (+ (best (- i 1) (- weight item-weight)) item-value)))])
                    (vector-set! (vector-ref memo i) weight result)
                    result
                    result)))))
      (best (- len 1) capacity))))
