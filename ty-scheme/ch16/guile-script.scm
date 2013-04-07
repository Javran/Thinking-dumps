#!/usr/bin/env guile
!#
; check this link:
; http://stackoverflow.com/questions/14126397/how-do-i-properly-execute-a-program-with-scheme48/
; some info about "program-arguments":
; http://www.gnu.org/software/guile/manual/html_node/Runtime-Environment.html
(load "../common/guile/utils.scm")

(out "nice boat!")
(out "Program arguments:")

(map (lambda (item) (out item)) (cdr (program-arguments)))
