(import (rnrs))

(define (knapsack capacity weights values)
  (let ([ws (list->vector weights)]
        [vs (list->vector values)])
    (let* ([len (vector-length ws)]
           [fv
            (let ([v (make-vector (+ len 1))])
              (let fill ([i 0])
               (if (< i len)
                   (begin
                     (vector-set! v i (make-vector (+ capacity 1) 0))
                     (fill (+ i 1)))
                   v)))])
      (let loopi ([i 0])
        (if (= i len)
            'done
            (let ([item-weight (vector-ref ws i)]
                  [item-value (vector-ref vs i)])
              (let loopw ([w 0])
                (if (> w capacity)
                    (loopi (+ i 1))
                    (begin
                      (vector-set! (vector-ref fv i) w
                                   (if (> item-weight w)
                                       (if (> i 0) (vector-ref (vector-ref fv (- i 1)) w) 0)
                                       (if (> i 0)
                                           (max
                                            (vector-ref (vector-ref fv (- i 1)) w)
                                            (+ (vector-ref (vector-ref fv (- i 1)) (- w item-weight)) item-value))
                                           (max
                                            0
                                            item-value))))
                      (loopw (+ w 1))))))))
      (if (= len 0)
          0
          (vector-ref (vector-ref fv (- len 1)) capacity)))))
