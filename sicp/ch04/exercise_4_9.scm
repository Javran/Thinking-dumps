(load "../common/utils.scm")
(load "../common/test-utils.scm")

; I'll only try to implement a customed `for` syntax.
;   as these statements are too unintuitive
;   to be either implemented or used.

; (for <var> <from> <pred?> <next>
;    <body>)
; =>
; (let <loop-proc> ((<var> <from>))
;   (if <pred?>
;     (begin
;       <body>
;       (<loop-proc> <next>))
;     <var>))
(define (desugar-for exp)
  (define loop-proc 'loop-proc)
  (define var            (cadr exp))
  (define from-exp       (caddr exp))
  (define pred?-exp (car (cdddr exp)))
  (define next-exp (cadr (cdddr exp)))
  (define body-seq (cddr (cdddr exp)))

  (list
    'let loop-proc
         (list (list var from-exp))
       (list 'if pred?-exp
             (cons 'begin
                   (append
                     body-seq
                     (list (list loop-proc next-exp))))
             var)))

(define (eval-for exp env)
  (eval (desugar-for exp) env))

(eval-for
  '(for i 1 (<= i 10) (+ i 1)
        (display i)
        (newline))
  user-initial-environment)
; print 1 .. 10

; yes, it's useless.
;   I don't know how to come up with a useful one
;   maybe the side effects can be modeled
;   but the syntax will be much more complex

(end-script)
