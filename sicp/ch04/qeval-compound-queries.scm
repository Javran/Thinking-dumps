;; conjunctions
(define empty-conjunction? null?)
(define first-conjunct car)
(define rest-conjuncts cdr)

;; conjunction handler
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      ;; for empty, always true
      frame-stream
      ;; otherwise, destruct the list,
      ;; "qeval" its head and recursively perform it on the rest
      ;; of the list
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(put 'and 'qeval conjoin)

;; disjunctions
(define empty-disjunction? null?)
(define first-disjunct car)
(define rest-disjuncts cdr)

;; disjunction handler
(define (disjoin disjuncts frame-stream)
  (if (empty-conjunction? disjuncts)
      ;; if the list is empty, always false
      the-empty-stream
      ;; merge together all possible frames
      ;; by "qeval" on each of them
      (interleave-delayed
       (qeval (first-disjunct disjuncts)
              frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

(put 'or 'qeval disjoin)

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
