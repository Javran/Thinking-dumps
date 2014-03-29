(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./3_5_4_streams_and_delayed_evaluation_common.scm")

(let ((inv-precision 10000))
  (out (stream-ref (solve identity 1 (/ 1.0 inv-precision)) inv-precision))
  (out (exp 1)))

(end-script)
