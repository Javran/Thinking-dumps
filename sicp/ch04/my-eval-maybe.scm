; Local variables:
; proc-entry: "./my-eval.scm"
; End:

; try to introduce Maybe from Haskell

; (just a) wrap a value `a` inside,
;   which might stand for
;   a successful computation result
(define (just a)
  (cons 'just a))

(define (just? maybe)
  (and (non-empty? maybe)
       (eq? 'just (car maybe))))

; `nothing` contains no value,
;   which might suggest the compuatation
;   has failed or the value is not present.
; (in order to fit into the nature of scheme
;   here nothing is just simply #f,
;   which can be used as a component
;   in `or` and `and` form)
; e.g.
; (or <try-first-time>
;     <try-second-time>
;     nothing)
; which is equivalent to:
; (or <try-first-time>
;     <try-second-time>)
; but IMHO slightly improves the readability
(define nothing
  #f)

(define from-just cdr)

; (maybe <success-f> <failure-f>)
;   returns a dispatcher,
;   the dispatcher accepts a maybe-value
;   if it's of form (just a), then apply
;   `success-f` to it, else `(failure-f)`
;   is returned.
(define (maybe success-f failure-f)
  (lambda (a-maybe)
    (if (just? a-maybe)
        ; just a
        (success-f (from-just a-maybe))
        ; nothing
        (failure-f))))

