#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "../../eopl3/chapter4/explicit-refs/top.scm")
(require "../../eopl3/chapter4/explicit-refs/data-structures.scm")

; the original version
(collect-garbage)
(time (test-all))
