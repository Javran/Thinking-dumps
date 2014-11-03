(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_lower_patch.scm")

(do-machine-test
 '((assign ptr-1 (op to-pointer) (const 20))
   (assign ptr-2 (op ptr-inc) (reg ptr-1))
   (perform (op vector-set!) (reg the-cars) (reg ptr-1) (const 1001))
   (perform (op vector-set!) (reg the-cdrs) (reg ptr-2) (const 2002))
   (assign b (op vector-ref) (reg the-cdrs) (reg ptr-2))
   (assign a (op vector-ref) (reg the-cars) (reg ptr-1))
   )
 '((a 1001)
   (b 2002)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
