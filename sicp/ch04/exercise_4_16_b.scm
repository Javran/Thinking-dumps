(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./my-eval.scm")

; I will only eliminate the first layer internal definition
;   because making the transformation recursive
;   is far beyond complicated.

(define (scan-out-defines p-body)
  (define (intern-define? exp)
    (and (non-empty? exp)
         ((list-tagged-with 'define) exp)))

  (define intern-def-exps
    (filter
      intern-define?
      p-body))
  (define intern-def-vars
    (map definition-variable intern-def-exps))
  (define intern-def-vals
    (map definition-value    intern-def-exps))
  (define (def->set exp)
    (if (intern-define? exp)
      `(set! ,(definition-variable exp)
             ,(definition-value    exp))
      exp))
  `(let 
     ; generate var-unassigned pairs
     ,(map (lambda (var)
             `(,var '*unassigned*))
           intern-def-vars)
     ; let body
     ,@(map def->set p-body)))

(do-test
  scan-out-defines
  (list
    (mat
      `((define u val1)
        (define v val2)
        (something)
        (foo))
      ; =>
      `(let ((u '*unassigned*)
             (v '*unassigned*))
         (set! u val1)
         (set! v val2)
         (something)
         (foo)))
    (mat
      `((define a a)
        (define b b)
        (foo1)
        (define c c)
        (bar1)
        foo2
        (define d d)
        bar2
        (a b c d))
      ; =>
      `(let ((a '*unassigned*)
             (b '*unassigned*)
             (c '*unassigned*)
             (d '*unassigned*))
         (set! a a)
         (set! b b)
         (foo1)
         (set! c c)
         (bar1)
         foo2
         (set! d d)
         bar2
         (a b c d)))
    )
  equal?)

(end-script)
