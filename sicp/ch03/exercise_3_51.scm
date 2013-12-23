(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define (show x)
  (display x) (newline)
  x)

(define x
  ; not perfect `lazy`
  ;   as when we comment out `(test)`, we still get a `0` as output.
  (stream-map
    show
    (stream-enumerate-interval 0 10)))

(define (tests)
  (stream-ref x 5)
  (stream-ref x 7))

; expected output would be:
#|
0
1
2
3
4
5
6
7
|#

; uncomment to see the output
; (tests)

(end-script)
