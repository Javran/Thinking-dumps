(define (simple-query query-pattern frame-stream)
  (stream-intermap
   (lambda (frame)
     (stream-append-delayed
      ;; search against assertions
      (find-assertions query-pattern frame)
      ;; search against rules
      (delay (apply-rules query-pattern frame))))
   frame-stream))

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

;; "not" query accessor
(define negated-query car)

(define (negate operands frame-stream)
  (stream-intermap
   (lambda (frame)
     (if (stream-null?
          ;; try to satisfy the query being negated.
          (qeval (negated-query operands)
                 (singleton-stream frame)))
         ;; if we cannot find any answer, then this query
         ;; itself is successful
         (singleton-stream frame)
         ;; else we return an empty stream to indicate the failure
         the-empty-stream))
   frame-stream))

(define (execute exp)
  (apply (eval (predicate exp)
               user-initial-environment)
         (args exp)))

;; lisp-value query accessors
(define predicate car)
(define args cdr)

;; TODO: still not sure about "instantiate-exp"
(define (lisp-value call frame-stream)
  (stream-intermap
   (lambda (frame)
     (if (execute
          (instantiate-exp
           call
           frame
           (lambda (v f)
             (error "Unknown pat var: LIST-VALUE"
                    v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

;; an "always-true" special form always returns the stream
;; without additional constraints
(define (always-true ignore frame-stream) frame-stream)

(define (install-handlers)
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true)
  'ok)

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
