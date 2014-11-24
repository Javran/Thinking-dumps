(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")
(load "./legacy_gc_patch.scm")

(load "./gc-test-machine.scm")

(let ((m (make-and-execute
          test-machine
          '())))
  ;; sum . map (* 2^8) $ [1..128] = 2113536
  (out (get-register m 'result)))

(end-script)
