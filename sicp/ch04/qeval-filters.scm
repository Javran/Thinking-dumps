;; dependencies:
;; - qeval-stream
;; - qeval-get-put
;; - qeval-base

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

(put 'not 'qeval negate)

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

(put 'lisp-value 'qeval lisp-value)

;; an "always-true" special form always returns the stream
;; without additional constraints
(define (always-true ignore frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
