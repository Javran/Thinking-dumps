(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./3_5_4_streams_and_delayed_evaluation_common.scm")

; to show that there is no circular dependency on values,
;   let `a -> b` denote "`a` requires the value of `b`"
; vc1 --> dvc0 --> il0
; il1 --> dil0 --> vc0
;              \-> il0

; we first should come with a not-working version,
;   and then we eliminate the circular dependency issues.
#|
(define (RLC R L C dt)
  (define (RLC-circuit-stream vc0 il0)
    (define vc (integral dvc vc0 dt))
    (define dvc (scale-stream il (/ -1.0 C)))
    (define il (integral dil il0 dt))
    (define dil (add-streams
                  (scale-stream vc (/ 1.0 L))
                  (scale-stream il (exact->inexact (/ (- R) L)))))
    (stream-map cons vc il))
  RLC-circuit-stream)
|#

; * rearrange the definitions so that `dvc` and `dil`
;   come after `vc` and `il`.
; * make `dvc` and `dil` delayed, which is required by `integral`
(define (RLC R L C dt)
  (define (RLC-circuit-stream vc0 il0)
    (define vc (integral (delay dvc) vc0 dt))
    (define il (integral (delay dil) il0 dt))
    (define dvc (scale-stream il (/ -1.0 C)))
    (define dil (add-streams
                  (scale-stream vc (/ 1.0 L))
                  (scale-stream il (exact->inexact (/ (- R) L)))))
    (stream-map cons vc il))
  RLC-circuit-stream)

(end-script)
