(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu_utils.scm")
(load "ec-prim.scm")

;; based on exercise 4.16
;; only scans definitions directly appear
;; in the procedure body
;; TODO: I think we can do something recursive
;; to go into deeper internal definitions
(define (scan-out-defines p-body)
  (define intern-def-exps
    (filter
     intern-define?
     p-body))
  (define intern-def-vars
    (map definition-variable intern-def-exps))
  (define intern-def-vals
    (map definition-value    intern-def-exps))
  (define (def->set exp)
    (if (definition? exp)
        `(set! ,(definition-variable exp)
               ,(definition-value    exp))
        exp))
  `(let
       ;; generate var-unassigned pairs
       ,(map (lambda (var)
               `(,var '*unassigned*))
             intern-def-vars)
     ;; let body
     ,@(map def->set p-body)))

(end-script)
