;; Let's try to create a better machine simulator
;; and name it "simu"

(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; toggle tests
(define *simu-test* #t)

(load "./simu_utils.scm")
(load "./simu_assemble.scm")
(load "./simu_test.scm")

;; TODO: run previous controller-text to verify this implementation

;; Local variables:
;; proc-entry: ""
;; End:
