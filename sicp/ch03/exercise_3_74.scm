(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(load "./exercise_3_74_common.scm")

(define zero-crossings
  (make-zero-crossings sense-data 0))

(print-few
  12 
  ((zip-streams-with list)
   sense-data
   zero-crossings))

(define zero-crossings-1
  (stream-map
    sign-change-detector
    sense-data
    ; make sense-data delayed by 1 element.
    (cons-stream
      0
      sense-data)))

(print-few
  12 
  ((zip-streams-with list)
   sense-data
   zero-crossings-1))


(end-script)
