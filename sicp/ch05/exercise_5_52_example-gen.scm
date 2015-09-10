(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")

;; TODO: tmp file for generating some examples to work with

(for-each
 out
 (statements
  (compile
   '(+ 10 (* 20 30) (+ 5 6))
   'val 'next)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
