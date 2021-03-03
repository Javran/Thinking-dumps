(import (rnrs))

(define (knapsack capacity weights values)
  (let ([ws (list->vector weights)]
        [vs (list->vector values)])
    (let ([len (vector-length ws)])
      (define (best i weight)
        ;; (best i weight) searches from 0th to i-th item and return the maximum value
        ;; with an exact weight.
        (if (or (< i 0) (<= weight 0))
            0
            (let ([item-weight (vector-ref ws i)]
                  [item-value (vector-ref vs i)])
              (if (> item-weight weight)
                  (best (- i 1) weight)
                  (max
                   (best (- i 1) weight)
                   (+ (best (- i 1) (- weight item-weight)) item-value))))))
      (best (- len 1) capacity))))

(display (knapsack
          750
          '(70 73 77 80 82 87 90 94 98 106 110 113 115 118 120)
          '(135 139 149 150 156 163 173 184 192 201 210 214 221 229 240))) (newline)
