(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "exercise_5_41_common.scm")

;; TODO: lexical address's structure
;; is previously (<frame-layer> . <position>)
;; and we need to fix this.

(let ((test-ctenv '((y z)
                    (a b c d e)
                    (x y)
                    )))
  (do-test
   find-variable
   (list
    (mat 'c test-ctenv '(1 2))
    (mat 'x test-ctenv '(2 0))
    (mat 'w test-ctenv 'not-found))))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
