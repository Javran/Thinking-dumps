#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

(test-all)

(out
  (expval->num
    (run "
let times3 = traceproc (n) -(-(n,-(0,n)),-(0,n))
in (times3 (times3 4))
")))
