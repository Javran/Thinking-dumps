(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; not sure how the system will be implemented yet,
;; so we just move codes from the book to here
;; hoping "my-eval" can be somehow reused to form this new system

(define input-prompt "qeval> ")
(define output-prompt "")

;; walk through list or improper list
;; using "leaf?" to tell whether the current node is a leaf
;; reserving structure.
;; TODO: try to use this function to replace things like "tree-walk"
(define (pair-map leaf? proc def-proc p)
  (define (pair-walk p)
    (cond ((leaf? p)
           (proc p))
          ((null? p)
           nil)
          ((pair? p)
           (cons (pair-walk (car p))
                 (pair-walk (cdr p))))
          (else (def-proc p))))
  (pair-walk p))

;; to "instantiate" an expression is
;; to replace variables with their values
(define (instantiate-exp exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 ;; call handler if the value cannot be found
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp))
                 (copy (cdr exp))))
          (else exp)))
  (copy exp))

;; TODO: test it later...
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (out "Assertion added to database.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate-exp
                q
                frame
                (lambda (v f)
                  (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))

;; evaluate assertions / rules / queries
;; on a stream of frames
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

;; Local variables:
;; proc-entry: ""
;; End:
