(load "./qeval-transform.scm")

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

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
