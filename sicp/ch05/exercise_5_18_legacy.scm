(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")
(load "./exercise_5_18_legacy_regtrace_patch.scm")

(let ((r (make-register "test-reg")))
  (register-trace-on! r)
  (set-contents! r 10)
  (set-contents! r 20)
  (register-trace-off! r)
  (set-contents! r 30)
  (register-trace-on! r)
  (set-contents! r 40))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
