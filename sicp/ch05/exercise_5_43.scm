(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu_utils.scm")
(load "ec-prim.scm")

#|
;; what happens when we have the following expression:
(lambda (x)
  (begin
    (define y 1)
    (define z 2)
    (if (= x 0)
        (begin
          (define a 10)
          (+ x a))
        (begin
          (define b 320)
          (* x 3 b)))))

;; TODO
;; conclusion: we have to go deeper
;; until we can reach another lambda-subexpression
|#

;; based on exercise 4.16
;; only scans definitions directly appear
;; in the procedure body
;; TODO: I think we can do something recursive
;; to go into deeper internal definitions

;; to make an almost-correct local definition
;; eliminator, we basically need an sexp to sexp
;; transformer for each form of s-exp:
;; * self-evaluating?
;; * quoted?
;; * variable?
;; * assignment?
;; * definition?
;; * if?
;; * lambda?
;; * begin?
;; * cond?
;; * let? (derived form)
;; * application?
;; we shouldn't assume the derived form is expanded,
;; as the transformation might introduce s-expressions in derived
;; form which would require expansion.

;; I think it's a good idea to make explicit the idea that
;; the abstract syntax tree is a functor.
(define (map-sexp f exp)
  'todo)

(define (scan-out-defines p-body)
  ;; p-body is a sequence of expression
  (define internal-definition
    (filter definition? p-body))

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
