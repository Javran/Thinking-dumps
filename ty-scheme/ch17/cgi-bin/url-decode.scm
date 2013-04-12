#!/usr/bin/env guile

!#

(load "cgi.scm")

(define query-string
  (let ((q-string (getenv "QUERY_STRING")))
    (if (not q-string)
      ; simulate a query-string if we don't have it
      "envvar=(N/A)"
      q-string)))

(display "content-type: text/plain\r\n\r\n")
(display "The query string is:") (newline)
(display query-string)(newline)(newline)
(display "The decoded query string is:") (newline)
(display (url-decode query-string))(newline)(newline)
(display "You can put any encoded string after '?' in the address bar and run to see the decoded string.")
