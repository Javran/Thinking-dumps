#lang eopl

(require "../common.rkt")
(require "../test-utils.rkt")

(require (prefix-in cbneed: "../eopl3/chapter4/call-by-need/top.scm"))
(require (prefix-in cbneed: "../eopl3/chapter4/call-by-need/data-structures.scm"))

(require (prefix-in cbname: "./call-by-name/top.rkt"))
(require (prefix-in cbname: "./call-by-name/data-structures.rkt"))

(define proc-str "
  let a = 1 in
  let f = proc (x) begin x; x end in
  (f begin set a = -(a,1); a end)
  ")

(out
  (cbname:expval->num
    (cbname:run proc-str)))
; -1
; the result is memoized,
;   which prevents the `set` effect from happening again

(out
  (cbneed:expval->num
    (cbneed:run proc-str)))
; 0
