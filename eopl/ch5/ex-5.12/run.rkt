#lang eopl

(require "../../common.rkt")
(require "../../test-utils.rkt")

(require "./top.rkt")
(require "./data-structures.rkt")

(instrument-cont #t)

(test-all)

(newline)

(run "-(-(44,11),3)")
