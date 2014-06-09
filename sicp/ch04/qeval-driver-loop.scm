(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; not sure how the system will be implemented yet,
;; so we just move codes from the book to here
;; hoping "my-eval" can be somehow reused to form this new system

(define input-prompt "qeval> ")
(define output-prompt "")

(define (instantiate-exp exp frame unbound-var-handler)
  (define (copy exo)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp))
                 (copy (cdr exp))))
          (else exp)))
  (copy exp))

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

;; needs data-directed dispatching supports here
(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (simple-query query frame-stream))))

;; Local variables:
;; proc-entry: ""
;; End:

