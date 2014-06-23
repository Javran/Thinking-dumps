;; dependencies
;; - qeval-database
;; - qeval-stream

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match
          (cdr pat)
          (cdr dat)
          (pattern-match (car pat)
                         (car dat)
                         frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match
         (binding-value binding) dat frame)
        (extend var dat frame))))

;; do pattern matching to tell if the pattern matches
;; with the assertion
(define (check-an-assertion
         assertion
         query-pat
         query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

;; extend the frame by using "pattern" to
;; search the database.
;; return a stream of frames that satisfies all assertions
(define (find-assertions pattern frame)
  (stream-intermap
   (lambda (datum)
     (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
