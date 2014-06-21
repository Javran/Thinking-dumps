(load "./qeval-base.scm")
(load "./qeval-transform.scm")
(load "./qeval-get-put.scm")

;; a big stream of all the assertions
(define THE-ASSERTIONS the-empty-stream)

;; a simple optimization:
;; if the patten begins with a constant symbol,
;; we might only need to search assertions with the same index.
;; Otherwise, if the pattern does not satisfy this criteria,
;; we instead return all the assertions from the system.
;; Further optimization is still possible.

(define (use-index? pat)
  (constant-symbol? (car pat)))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

;; TODO: move to get-put
;; get a stream, return an empty stream if not found
(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

;; assertion is indexed in a way that
;; the first key is the constant symbol,
;; and the second key is the symbol "assertion-frame"
(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-all-assertions) THE-ASSERTIONS)

;; "frame" is not use in the body of "fetch-assertions"
;; not sure what's the point of having this parameter.
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

;; rules are also big streams
(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)
(define (get-indexed-rules pattern)
  (stream-append
   ;; different from assertions, rules might have variables
   ;; in conclusion, we put all possible rules together to form another stream
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

;; add rules or assertions to database
(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream
                assertion
                current-assertion-stream))))))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
