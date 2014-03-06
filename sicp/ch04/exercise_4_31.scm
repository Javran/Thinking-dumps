(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./my-eval.scm")
(load "./exercise_4_31_promise.scm")

(load "./exercise_4_31_delay_force.scm")

(test-promise)

(install-eval-delay)
(install-eval-force)

(newline)
(out "===== new extensions are inserted, retesting ...")

(my-eval-test-all)

;; my approach:
;; * use it as an extended `define` rather than
;;   modify `define`, in this case,
;;   I call this form `define-eaa`
;;   ("eaa" for "extended argument annotation")
;;
;; * this should just be some derived form,
;;   and the programmer takes the responsibility of
;;   dealing with the evaluation strategy explicitly as well
;;   since we don't know how a specified argument will be used.
;;   Usage:
;;
;;   (define-eaa (f a (b lazy) c (d lazy-memo))
;;     .. a ..
;;     .. (b) ..
;;     .. c ..
;;     .. (force d) ..)
;;
;;   The idea is, when it comes to function application
;;
;;   * for call-by-value arguments, do nothing
;;   * for call-by-name arguments, wrap it inside a lambda
;;   * for call-by-need arguments, use `delay`
;;
;;   Let's stick to the idea of implementing this support as
;;   a simple extension and make no effort to modify essential
;;   procedures like `my-eval` or `my-apply`.
;;
;;   One way of achieving this might be changing the way an `eaa` procedure
;;   is called. Here I'll write another special form `call-eaa`,
;;   all procedures defined with `define-eaa` will have to be applied using
;;   this function rather than relying on `my-apply` and `my-eval`.
;;
;;   the function application will be transformed following these rules:
;;
;;   * (call-eaa proc a) => (proc a)             ; for call-by-value
;;   * (call-eaa proc a) => (proc (lambda () a)) ; for call-by-name
;;   * (call-eaa proc a) => (proc (delay a))     ; for call-by-need
;;
;;   so the application of the procedure in the exercise:
;;
;;   (proc a b c d)
;;
;;   will look like:
;;
;;   (call-eaa proc a b c d)
;;
;;   which will eventually be transformed to:
;;
;;   (proc a (lambda () b) c (delay d))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
