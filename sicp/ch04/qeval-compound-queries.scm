;; building blocks are still missing
;; we need to come back to this file later...

;; conjunction handler
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

;; disjunction handler
(define (disjoin disjuncts frame-stream)
  (if (empty-conjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts)
              frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))
