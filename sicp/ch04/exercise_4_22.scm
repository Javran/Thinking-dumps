(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./my-eval.scm")

;; this part is already done in file:
;; ./my-eval-e-let.scm
(out (my-eval
      `(let ((a 10)
             (b 20)
             (c 30))
         (+ a b c))
      (init-env)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
