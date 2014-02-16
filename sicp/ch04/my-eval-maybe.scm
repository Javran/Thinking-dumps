; Local variables:
; proc-entry: "./my-eval.scm"
; End:

; try to introduce Maybe from Haskell

; (just a) wrap a value `a` inside,
;   which might stand for
;   a successful computation result
(define (just a)
  (cons 'just a))

; `nothing` contains no value,
;   which might suggest the compuatation
;   has failed or the value is not present.
(define nothing
  #f)

(define from-just cdr)

; (maybe <success-f> <failure>)
;   returns a dispatcher,
;   the dispatcher accepts a maybe-value
;   if it's of form (just a), then apply
;   `success-f` to it, else `failure`
;   is returned.
(define (maybe success-f failure)
  (lambda (a-maybe)
    (if a-maybe
        ; just a
        (success-f (from-just a-maybe))
        ; nothing
        failure)))

