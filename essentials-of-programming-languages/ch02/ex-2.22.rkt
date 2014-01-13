#lang eopl

(require "./common.rkt")

; interfaces:
; * empty-stack
; * push
; * pop
; * top
; * empty-stack?

; test if something is a SchemeVal
;   assume all possible values are acceptable.
(define scheme-val? (const #t))

(define-datatype stack stack?
  (stack-none)
  (stack-some
    (top scheme-val?)
    (rest stack?)))

; empty-stack?: Stack -> Bool
(define (empty-stack? s)
  (cases stack s
    (stack-none () #t)
    (stack-some (t r) #f)))

; push: SchemeVal x Stack -> Stack
(define (push x s)
  (stack-some x s))

; pop: Stack -> Stack
(define (pop s)
  (cases stack s
    (stack-none ()
      (eopl:error 'pop
        "popping on empty stack"))
    (stack-some (x s)
      s)))

; top: Stack -> SchemeVal
(define (top s)
  (cases stack s
    (stack-none ()
      (eopl:error 'top
        "no top element for an empty stack"))
    (stack-some (x xs)
      x)))

(define empty-stack stack-none)

(define s1
  (empty-stack))

(define s2
  ; push 2
  ; push 3
  ; pop
  ; push 4
  ; would look like:
  ; 4,2 when popping
  (push 4
    (pop
      (push 3
        (push 2
          (empty-stack))))))

; will cause error when uncommented
; (out (pop s1)
;      (top s1))

(out (top s2)
     ; 4
     (top (pop s2))
     ; 2
     (empty-stack?  (pop (pop s2)))
     ; #t
     )
