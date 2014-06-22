(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define *qeval-tests* #t)

(load "./qeval-base.scm")
(load "./qeval-transform.scm")
(load "./qeval-frames.scm")

(load "./qeval-get-put.scm")
(load "./qeval-stream.scm")
(load "./qeval-database.scm")
;; the following code is still unconfirmed
;; this should be the main file.

(define (find-assertions pattern frame)
  (stream-flatmap
   (lambda (datum)
     (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (check-an-assertion
         assertion
         query-pat
         query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

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



;; Local variables:
;; proc-entry: ""
;; End:
