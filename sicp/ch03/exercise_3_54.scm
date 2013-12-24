(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define mul-streams
  (zip-streams-with *))

(define factorials
  (cons-stream
    1
    (mul-streams
      factorials
      (stream-cdr integers))))

(for-each
  (lambda (n)
    (format #t
      "~A -> ~A~%"
      (+ n 1)
      (stream-ref factorials n)
      ))
  (list-in-range 0 9))

(end-script)
