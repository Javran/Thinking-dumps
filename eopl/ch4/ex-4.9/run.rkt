#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

; the modified version
(collect-garbage)
(time (test-all))
; cpu time: 163 real time: 162 gc time: 0
; cpu time: 164 real time: 164 gc time: 0
; cpu time: 160 real time: 160 gc time: 0
; cpu time: 160 real time: 160 gc time: 0
; cpu time: 164 real time: 163 gc time: 0
