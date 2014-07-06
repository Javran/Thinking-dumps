;; pattern matching between `pat` and `dat`,
;; `dat` shouldn't contain variables
(define (pattern-match pat dat frame)
  ;; test and extend the existing frame with binding (var, dat) if
  ;; it is consistent
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

;; extend the frame by using "pattern" to
;; search the database.
;; return a stream of frames that satisfies all assertions
(define (find-assertions pattern frame)
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

  (stream-intermap
   (lambda (datum)
     (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (qeval-pattern-matching-tests)
  ;; pattern-match and extend-if-consistent
  ;; are mutual recursive functions.
  ;; we test pattern-match only but hopefully
  ;; it should also have a nice coverage on extend-if-consistent
  (do-test
   pattern-match
   (list
    ;; case #1: fresh variable on the left side
    (mat 'x 'x 'failed 'failed)
    (mat 'x 'y 'failed 'failed)
    (mat '(() a (b c d))
         '(() a (b c d))
         empty-frame
         empty-frame)
    (mat '(? x) '(a b c d) empty-frame
         (alist->frame '(((? x) . (a b c d)))))
    (mat '(? x) '(? y) empty-frame
         (alist->frame '(((? x) . (? y)))))
    ;; for assertions, there shouldn't be variables on the right hand side
    (mat '((? x) a b c) '(d a (? y)) (alist->frame '( ((? y) . (b c)) ))
         'failed)
    (mat '((? x) a . (? y)) '(d a b c) (alist->frame '( ((? z) . whatever) ))
         (alist->frame '( ((? x) . d)
                          ((? y) . (b c))
                          ((? z) . whatever) )))
    ;; case #2: bound variable on the left side
    (mat '(? x) '(a b c) (alist->frame '(( (? x) . (a b c))))
         (alist->frame '(( (? x) . (a b c)))))
    (mat '(? x) '(a b c) (alist->frame '(( (? x) . (a . (? y)) )))
         (alist->frame '(( (? x) . (a . (? y)))
                         ( (? y) . (b c)))))
    ;; cannot match '(a b) against '(a b <?>)
    (mat '(? x) '(a b) (alist->frame '(( (? x) . (a b (? y)))))
         'failed)
    )
   result-frame-equal?)

  'ok)

(if *qeval-tests*
    (qeval-pattern-matching-tests)
    'ok)

;; Local variables:
;; proc-entry: "./qeval.scm"
;; End:
