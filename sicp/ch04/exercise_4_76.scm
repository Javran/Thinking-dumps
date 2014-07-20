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

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
