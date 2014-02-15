#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

(test-all)

; this modification does not require anything,
;   because Bounce itself is
;   either an ExpVal or an (() -> Bounce)
;   therefore (lambda () ExpVal) and ExpVal
;   will have the same outcome
