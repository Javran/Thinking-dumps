(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_lower_patch.scm")
(load "./rewrite.scm")
(load "./simu_listprim2_patch.scm")

(define simple-instructions
  '((assign a (op cons) (const 2) (const ()))
    (assign b (op cons) (const 1) (reg a))
    (assign c (op cons) (const 0) (reg b))
    (assign d (reg c))))

(for-each out simple-instructions)
(newline)

(for-each out (rewrite-instructions
               list-primitives-rules
               simple-instructions))

(define fake-instruction-list
  `(aaa
    bbb
    ccc))

(define fake-rules
  `( (aaa
      (from-aaa
       ddd
       eee
       fff))
     (bbb
      (from-bbb
       aaa
       ccc
       kkk))
     (ccc
      (from-ccc))
     (eee
      (from-eee))
     (fff
      (from-fff
       kkk))
     (from-eee
      (from-eeee))))

(assert (equal?
         (rewrite-instructions* fake-rules fake-instruction-list)
         '(;; ==== aaa expands to
           from-aaa
           ddd
           ;; ==== eee => from-eee -> from-eeee
           from-eeee
           ;; ==== fff => from-fff; kkk
           from-fff
           kkk
           ;; ==== bbb expands to
           from-bbb
           ;; ==== aaa => from-aaa; ddd; from-eeee; from-fff; kkk
           from-aaa
           ddd
           from-eeee
           from-fff
           kkk
           ;; ==== ccc => from-ccc
           from-ccc
           kkk
           ;; ==== ccc expands to
           from-ccc)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
