#!/usr/bin/env guile

!#

; returns the leftmost index of c in string s, returns #f elsewise
(define string-index
  (lambda (s c)
    (let ((n (string-length s)))
      (let loop ((i 0))
        (cond
          ((>= i n) #f)
          ((char=? (string-ref s i) c) i)
          (else (loop (+ i 1))))))))

; break string s into substrings, the seperator is c
(define split
  (lambda (c s)
    (let loop ((s s))
      (if (string=? s "")
        ; nothing to do, return empty list
        '()
        ; elsewise
        (let ((i (string-index s c)))
          (if i
            ; found
            (cons (substring s 0 i)
                  (loop (substring s (+ i 1) (string-length s))))
            ; else, no more seperator, let s be the last element
            (list s)))))))

(define safe-getenv
  (lambda (var)
    (let ((envvar (getenv var)))
      (if envvar
        envvar
        "(N/A)"))))

(define query-string
  (let ((q-string (getenv "QUERY_STRING")))
    (if (not q-string)
      ; simulate a query-string if we don't have it
      "envvar=(N/A)"
      q-string)))

(define args
  (let ((parse-kvpair
          (lambda (raw-kv)
            (split #\= raw-kv)))
        (raw-kvpairs
          (split #\& query-string)))
    (map parse-kvpair raw-kvpairs)))
             

(define envvar
  (cadr (assoc "envvar" args)))

(display "content-type: text/plain\r\n\r\n")

(display "The query string is:") (newline)
(display query-string) (newline)
(display "Query result:") (newline)

(display envvar)
(display " = ")
(display (safe-getenv envvar)) (newline)
