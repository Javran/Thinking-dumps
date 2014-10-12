(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")
(load "./exercise_5_18_legacy_regtrace_patch.scm")

(let ((m (make-and-execute
          '(controller
            (assign n (const 10))
            (assign m (const 20))
            ;; turn on flag on "n" only
            (perform (op trace-reg-on) (const n))
            (assign n (const a))
            (assign m (const b))
            ;; turn on flag on "m", turn off flag on "n"
            (perform (op trace-reg-on) (const m))
            (perform (op trace-reg-off) (const n))
            (assign n (const aaa))
            (assign m (const bbb))
            ;; turn on flags on "m" an "n"
            (perform (op trace-reg-on) (const n))
            (perform (op trace-reg-on) (const m))
            (assign n (const 123))
            (assign m (const 456)))
          '())))
  (out "done"))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
