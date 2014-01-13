#lang eopl

(require "../common.rkt")

; interfaces:
; * empty-stack
; * push
; * pop
; * top
; * empty-stack?

; empty-stack: () -> Stack
(define (empty-stack)
  (list
    ; top impl
    (lambda ()
      (eopl:error
        'pop
        "the stack is empty"))
    ; empty-stack? impl
    (lambda () #t)
    ; pop impl
    (lambda ()
      (eopl:error
        'pop
        "the stack is empty"))))

; push: Stack x a -> Stack
(define (push stack x)
  (list
    ; top impl
    (lambda () x)
    ; empty-stack? impl
    (lambda () #f)
    ; pop impl
    (lambda () stack)))
    
; pop: Stack -> Stack
(define (pop stack)
  ((caddr stack)))

; empty-stack?: Stack -> Bool
(define (empty-stack? stack)
  ((cadr stack)))

; top: Stack -> a
(define (top stack)
  ((car stack)))

; test
(let ((s (empty-stack)))
  ; empty stack
  (assert (empty-stack? s))
  (assert (not (empty-stack? (push s 'x))))
  ; push an element
  (assert (eq? 'x (top (push s 'x))))
  ; push two elements and look at the top
  (assert (eq? 'y (top (push (push s 'x) 'y))))
  (assert (eq? 'x (top (pop (push (push s 'x) 'y)))))

  (out "stack test done")
  'done)
