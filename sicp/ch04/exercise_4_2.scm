(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (tagged-list? exp tag)
  ; lists beginning with a designated symbol
  ; refactored
  (and (non-empty? exp)
       (eq? (car exp) tag)))

; if we move `((application? exp) ...)` in `eval`
;   to the position before `((assignment? exp) ...)``
; we will need to change `application?`, `operator` and `operands`
;   to make it work again

(define (application? exp)
  ; applications are now of form: 
  ; (call <proc> . <args>)
  (tagged-list? exp 'call))

(define operator cadr)
(define operands cddr)

(define test-1 '(define x 3))
(define test-2 '(call factorial 3))
(define test-3 '(call + 1 2))
(define test-4 '(call proc))

(out (map application?
          (list test-1
                test-2
                test-3
                test-4)))
; #f #t #t #t

(out (map operator
          (list test-2
                test-3
                test-4)))
; factorial + proc

(out (map operands
          (list test-2
                test-3
                test-4)))
; (3) (1 2) ()

(end-script)
