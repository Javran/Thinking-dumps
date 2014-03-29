#lang racket

(require "./exercise_3_81_common.rkt")

; define two request sequences:

(define c-reqs-1
  '(reset 1
    generate
    generate
    generate
    generate
    reset 2
    reset 3
    generate
    reset 4
    generate
    generate
    reset 1
    generate))

(define c-reqs-2
  '(reset 100
    generate
    generate
    reset 100
    generate
    generate
    generate
    reset 101
    generate))

(display
  (format "Assertion:~%~
           Line 1 == Line 3~%~
           Line 2 == Line 4~%~
           For Line 1 & 3, first element == last element~%~
           For Line 2 & 4, first two elements == two elements proceeded~%"))

(for-each
  (lambda (line creqs)
    (display 
      (format
        "~A: ~A~%"
        line
        (stream->list
          (process-random-generator-crequests creqs)))))
  '(1 2 3 4)
  (list c-reqs-1
        c-reqs-2
        c-reqs-1
        c-reqs-2))
