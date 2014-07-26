(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; the first problem would be how to make frame pairs
;; since two streams might be infinite, we need to make it
;; possible for each resulting pair to be appeared in the stream
;; and we can use the same technique when we are dealing with
;; streams in chapter 3.

;; let's say this function is `combine`.
;; we break two streams into parts:
;; s1 = a:as, s2 = b:bs
;; therefore we can construct 4 streams:
;; 1. (a,b) : []
;; 2. (stream-map (a:) bs)
;; 3. (stream-map (:b) as)
;; 4. (combine as bs)
;; then use the "interleave" function to merge them into one big stream
(define (interleave s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (cons-stream
          (stream-car s1)
          (interleave s2 (stream-cdr s1))))))

(define (combine-with f)
  (define (combine-intern s1 s2)
    (if (or (stream-null? s1)
            (stream-null? s2))
        ;; we are "taking product" of two stream
        ;; if any of these streams does not have an element
        ;; so does the resulting stream
        the-empty-stream
        ;; else
        (let ((a  (stream-car s1))
              (as (stream-cdr s1))
              (b  (stream-car s2))
              (bs (stream-cdr s2)))
          (cons-stream
           (f a b)
           (interleave
            (interleave
             (stream-map
              (lambda (x)
                (f a x)) bs)
             (stream-map
              (lambda (x)
                (f x b)) as))
            (combine-intern as bs))))))
  combine-intern)

(out (stream->list
      ((combine-with cons)
       (list->stream '(a b c d))
       (list->stream (list-in-range 1 3)))))

;; A big problem here is that when a rule is applied,
;; the variable names are changed, since we are runing two sub-terms
;; separately, how can we tell the relationship between two frame bindings?

(load "./qeval.scm")

;; Some more notes:
;; pretend that we evaluate each conjunction with same frame,
;; therefore we are still sharing some of the variable bindings
;; And actually there is no confliction between variables
;; because we've transformed variables to make them unique

;; I think currently a potential method would be:
;; let first conjunction and rests of the conjunctions run separately
;; figure out some way to say that these two frames are "compatible"
;; here suppose first conjunction generates a binding `a` related to
;; the original binding `c`, and rest of the conjunctions also have a
;; binding `b`, now we need to come up with some ways to tell
;; if both `a ~ c` and `b ~ c`  can be satisfied.
;; if the "compatible check" fails, then we just remove that frame
;; from the stream

;; Here is a potential approach:
;; we only compare bindings in common
;; say that we are given two frames, which must share some bindings
;; we compare all the variables in common, and try to unify
;; the value of the same variable with two different frames
;; the unification is done on two frames, producing a new frame
;; and to merge these three frames together (some of the frames
;; must be exactly the same, remember we only add bindings to
;; the frame but not change existing ones

(define (merge-frames frame1 frame2)
  (let ((vars1 (map binding-variable frame1))
        (vars2 (map binding-variable frame2)))
    (let ((common-vars
           ;; the variables in common
           ;; are those variables in vars1
           ;; which also appears in vars2
           (filter
            (lambda (var)
              (member var vars2))
            vars1)))
      (let ((frame3
             ;; keep extending the new frame if possible
             (let loop ((vars common-vars)
                        (new-frame empty-frame))
               (if (eq? new-frame 'failed)
                   'failed
                   (if (null? vars)
                       new-frame
                       (loop
                        (cdr vars)
                        (unify-match2
                         (car vars1)
                         frame1
                         (car vars2)
                         frame2
                         new-frame)))))))
        (merge-with-new-frame
         frame1 frame2 frame3)))))

(define (unify-match2 v1 fr1 v2 fr2 newfr)
  (define (lookup k fr)
    ;; lookup the fresh bindings first
    ;; the actual frame is examined only
    ;; if there is no fresh binding for the
    ;; variable in question
    (or (binding-in-frame k newfr)
        (binding-in-frame k fr)))
  (let ((p1 (lookup v1 fr1))
        (p2 (lookup v2 fr2)))
    ;; we need to be careful here
    ;; since the same variable might be bound to
    ;; totally different things
    ;; therefore simply test `equal?` might go wrong
    (cond ((eq? newfr 'failed) 'failed)
          ((var? p1) (extend-if-possible-2
                      p1 fr1 p2 fr2 newfr))
          ((var? p2) (extend-if-possible-2
                      p2 fr2 p1 fr1 newfr))
          ;; now we can confirm that
          ;; both p1 and p2 are not variables
          ((and (pair? p1) (pair? p2))
           (unify-match2
            (cdr p1) fr1
            (cdr p2) fr2
            (unify-match2
             (car p1) fr1
             (car p2) fr2
             newfr)))
          ;; now we can confirm that
          ;; p1 and p2 are not both pairs
          ;; and I think it's safe to use "equal?" here
          ((equal? p1 p2) newfr)
          (else 'failed))))

;; TODO: merge-with-new-frame

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
