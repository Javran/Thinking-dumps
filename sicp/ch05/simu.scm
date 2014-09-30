;; Let's try to create a better machine simulator
;; and name it "simu"

(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; toggle tests
(define *simu-test* #f)

(load "./simu_utils.scm")
(load "./simu_assemble.scm")
(load "./simu_test.scm")

;; TODO:
;; I feel it is necessary to provide
;; an extra slot to make room for the
;; machine to carry extra information
;; should be an alist that stores info
;; in a key-value manner

;; Local variables:
;; proc-entry: ""
;; End:
