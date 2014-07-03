;; dependencies
;; - qeval-database
;; - qeval-stream
;; TODO: tests

(define (pattern-match pat dat frame)
        ;; the pattern matching has failed.
  (cond ((eq? frame 'failed) 'failed)
        ;; "pat" and "dat" matches exactly
        ((equal? pat dat) frame)
        ;; if the pattern is a variable, try to extend it
        ((var? pat) (extend-if-consistent pat dat frame))
        ;; recursively pattern matching
        ((and (pair? pat) (pair? dat))
         (pattern-match
          (cdr pat)
          (cdr dat)
          (pattern-match (car pat)
                         (car dat)
                         frame)))
        ;; for all the other cases, the pattern matching fails
        (else 'failed)))

;; test and extend the existing frame with binding (var, dat) if
;; it is consistent
;; TODO: some test cases?
(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        ;; if the "var" is already bound to something,
        ;; we try to do pattern matching between the value that "var" points to
        ;; and "dat"
        (pattern-match
         (binding-value binding) dat frame)
        ;; if "var" has not yet bound to any value, then we extend the current frame
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
