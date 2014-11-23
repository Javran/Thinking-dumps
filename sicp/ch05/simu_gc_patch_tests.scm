(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_gc_patch.scm")

(load "./gc-test-machine.scm")

(let ((m (build-and-execute
          test-machine
          '())))
  ;; sum . map (* 2^8) $ [1..128] = 2113536
  (out (machine-reg-get m 'result)))

(end-script)
