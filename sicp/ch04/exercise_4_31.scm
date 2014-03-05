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
;;   * for call-by-value arguments, do nothing
;;   * for call-by-name arguments, wrap it inside a lambda
;;   * for call-by-need arguments, use `delay`
;;
;;   `apply` should know exactly whether a procedure
;;   is created by `define-eaa`, and handle each of them properly
;;
;;   the function application will happen following these rules:
;;
;;   * (proc a) => (proc a)             ; for call-by-value
;;   * (proc a) => (proc (lambda () a)) ; for call-by-name
;;   * (proc a) => (proc (delay a))     ; for call-by-need
;;
;;   so the application of the procedure in the exercise:
;;
;;   (proc a b c d)
;;
;;   will be transformed to:
;;
;;   (proc a (lambda () b) c (delay d))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
