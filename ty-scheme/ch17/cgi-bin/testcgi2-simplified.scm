#!/usr/bin/env guile

!#

(load "cgi.scm")

(display "content-type: text/plain\r\n\r\n")

(parse-form-data)

(define envvar (form-data-get/1 "envvar"))

(display envvar)
(display " = ")
(display (getenv envvar))
(newline)
