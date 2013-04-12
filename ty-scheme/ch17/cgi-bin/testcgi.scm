#!/usr/bin/env guile

!#

(display "content-type: text/plain\r\n\r\n")

(display "CGI server is running.")
(newline)

(for-each
  (lambda (env-var)
    (display env-var)
    (display " = ")
    (display (or (getenv env-var) "(N/A)"))
    (newline))
  '("AUTH_TYPE"
    "CONTENT_LENGTH"
    "CONTENT_TYPE"
    "DOCUMENT_ROOT"
    "GATEWAY_INTERFACE"
    "HTTP_ACCEPT"
    "HTTP_REFERER" ; [sic]
    "HTTP_USER_AGENT"
    "PATH_INFO"
    "PATH_TRANSLATED"
    "QUERY_STRING"
    "REMOTE_ADDR"
    "REMOTE_HOST"
    "REMOTE_IDENT"
    "REMOTE_USER"
    "REQUEST_METHOD"
    "SCRIPT_NAME"
    "SERVER_NAME"
    "SERVER_PORT"
    "SERVER_PROTOCOL"
    "SERVER_SOFTWARE"))
