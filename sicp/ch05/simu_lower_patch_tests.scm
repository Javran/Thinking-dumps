(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_lower_patch.scm")

(do-machine-test
 '((assign ptr (op to-pointer) (const 20))
   (perform (op vector-set!) (reg the-cars) (reg ptr) (const 1001))
   (assign a (op vector-ref) (reg the-cars) (reg ptr))
   )
 '((a 1001)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
