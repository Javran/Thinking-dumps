(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (show x)
  (display x) (newline)
  x)

(define (stream-enumerate-interval low high)
  (if (> low high)
    nil
    (cons-stream low (stream-enumerate-interval
                       (+ low 1)
                       high))))

(define x
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
