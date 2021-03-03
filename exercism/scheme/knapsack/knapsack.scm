(import (rnrs))

(define (knapsack capacity weights values)
  (let ([len (length weights)])
    (let loopi ([i 0]
                [ws weights]
                [vs values]
                [v-prev (make-vector (+ capacity 1) 0)]
                [v-cur (make-vector (+ capacity 1))])
      ;; 0-1 knapsack problem.
      ;; let f[i+1, w] be the best possible value when picking from item[0..i] (ranges are all inclusive).
      ;; so we have:
      ;;
      ;;   f[i+1, w] =
      ;;     max of:
      ;;     - f[i, w] (always possible to skip current item i)
      ;;     - f[i, w - item[i].weight ] + item[i].value (only possible when w >= item[i].weight)
      ;;
      ;; note that f[i+1, _] solely depends on f[i, _], we can therefore cut space down to O(capacity) by
      ;; rotating two vectors of length capacity.
      ;;
      ;; current iteration computes v-cur,
      ;; in which v-cur[w] is the best value possible using item[0..i]
      ;; and v-prev[w] is the best value possible using items from [0..i-1]
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
          (let ([item-weight (car ws)]
                [item-value (car vs)])
            (let loopw ([w 0])
              (if (> w capacity)
                  ;; next iteration, rotate two vectors.
                  (loopi (+ i 1) (cdr ws) (cdr vs) v-cur v-prev)
                  (begin
                    (vector-set!
                     v-cur w
                     (if (> item-weight w)
                         ;; must skip
                         (vector-ref v-prev w)
                         (max
                          ;; may skip
                          (vector-ref v-prev w)
                          ;; may pick
                          (+ (vector-ref v-prev (- w item-weight))
                             item-value))))
                    (loopw (+ w 1))))))))))
