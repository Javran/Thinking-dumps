(load "../common/utils.scm")
(load "../common/test-utils.scm")

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

(out list-tagged-with)

(end-script)
