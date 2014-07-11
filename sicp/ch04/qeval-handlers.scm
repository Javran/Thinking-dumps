;; use pattern matching to find valid frames
(define (simple-query query-pattern frame-stream)
  (stream-intermap
   (lambda (frame)
     (stream-append-delayed
      ;; search against assertions
      (find-assertions query-pattern frame)
      ;; search against rules
      (delay (apply-rules query-pattern frame))))
   frame-stream))

;; conjunction handler
(define (conjoin conjuncts frame-stream)
  ;; conjunction accessors
  (define empty-conjunction? null?)
  (define first-conjunct car)
  (define rest-conjuncts cdr)

  (if (empty-conjunction? conjuncts)
      ;; for empty, always true
      frame-stream
      ;; otherwise, destruct the list,
      ;; "qeval" its head and recursively perform it on the rest
      ;; of the list
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

;; disjunction handler
(define (disjoin disjuncts frame-stream)
  ;; disjunction accessors
  (define empty-disjunction? null?)
  (define first-disjunct car)
  (define rest-disjuncts cdr)

  (if (empty-disjunction? disjuncts)
      ;; if the list is empty, always false
      the-empty-stream
      ;; merge together all possible frames
      ;; by "qeval" on each of them
      (interleave-delayed
       (qeval (first-disjunct disjuncts)
              frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

(define (negate operands frame-stream)
  ;; "not" query accessor
  (define negated-query car)

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

;; lisp-value handler: evaluate the pattern
;; as if it was lisp code
(define (lisp-value call frame-stream)
  ;; lisp-value query accessors
  (define predicate car)
  (define args cdr)

  ;; to execute an expression
  ;; is to evaluate it as if it was lisp code
  (define (execute exp)
    (apply (eval (predicate exp)
                 user-initial-environment)
           (args exp)))
  (stream-intermap
   (lambda (frame)
     (if (execute
          ;; fill in variables according to the frame
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

;; TODO: document
(define (lisp-eval call frame-stream)
  ;; format: (lisp-eval <func> <return-var> <arg1> <arg2> ...)
  ;; call = (<func> <return-var> <arg1> <arg2> ...)
  (define predicate car)
  (define return-pat cadr)
  (define args cddr)

  (define (execute exp)
    (apply (eval (car exp)
                 user-initial-environment)
           (cdr exp)))

  (stream-intermap
   (lambda (frame)
     (let* ((eval-result
             (execute
              ;; to avoid accidentally
              ;; instantiate return-val,
              ;; we just skip it.
              ;; so in procedure "execute"
              ;; we should instead use "car" and "cdr"
              ;; instead of field accessors for calls
              (instantiate-exp
               (cons (predicate call)
                     (args call))
               frame
               (lambda (v f)
                 (error "Unknown pat var: LIST-EVAL"
                        v)))))
            (match-result
             (pattern-match (return-pat call) eval-result frame)))
       (if (eq? match-result 'failed)
           the-empty-stream
           (singleton-stream match-result))))
   frame-stream))

(define (install-handlers)
  (put 'and 'qeval conjoin)
  (put 'or 'qeval disjoin)
  (put 'not 'qeval negate)
  (put 'lisp-value 'qeval lisp-value)
  (put 'always-true 'qeval always-true)
  (put 'lisp-eval 'qeval lisp-eval)
  'ok)

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
