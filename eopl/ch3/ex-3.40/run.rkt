#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

(run "letrec f(x) = -(x,1) in (f 33)")
(test-all)
