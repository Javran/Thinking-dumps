#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "../../eopl3/chapter4/explicit-refs/top.scm")
(require "../../eopl3/chapter4/explicit-refs/data-structures.scm")

; the original version
(collect-garbage)
(time (test-all))
; cpu time: 167 real time: 163 gc time: 4
; cpu time: 163 real time: 162 gc time: 4
; cpu time: 163 real time: 162 gc time: 0
; cpu time: 166 real time: 162 gc time: 3
; cpu time: 160 real time: 160 gc time: 4

