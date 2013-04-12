; please use Guile for this source file
(load "../common/guile/utils.scm")
(load "../common/guile/clock.scm")

(out *infinity*)
(out (= +inf.0 *infinity*))
; should be #t

(define my-interrupt-handler
  (lambda ()
    (out "my-interrupt-handler fired.")))

(clock 'set-handler my-interrupt-handler)
(clock 'set 1)

(sleep 10)

(clock 'set 20)

; force handler to run
(clock 'set 0)

; wait for a while
(sleep 1)

; deactive the clock
(clock 'set *infinity*)
