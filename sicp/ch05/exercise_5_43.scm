(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu_utils.scm")
(load "ec-prim.scm")
(load "exercise_5_23_common.scm")

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

(define (transform-sexp exp)
  ;; invariant:
  ;; * the inner-expressions are always transformed before
  ;;   its outer-expression
  ;; * the input is a valid s-exp
  ;;   and the output is a valid s-exp but without local definitions
  (cond
   ((or (self-evaluating? exp)
        (quoted? exp)
        (variable? exp))
    ;; forms that couldn't contain a sub-expression,
    ;; the transformation will just leave them unchanged
    exp)
   ((assignment? exp)
    ;; (set! <var> <exp>)
    `(set! ,(assignment-variable exp)
           ,(transform-sexp (assignment-value exp))))
   ((definition? exp)
    ;; We are not going to take two cases into account.
    ;; Instead, we "normalize" the definition so we are sure to
    ;; deal with a normalized form later
    ;; (this makes "lambda" the only form that the transformation cares about)
    (let ((exp (normalize-define exp)))
      ;; overwritten exp shadowing the original one
      `(define
         ,(definition-variable exp)
         ,(transform-sexp (definition-value exp)))))
   ((if? exp)
    ;; (if <pred> <cons> <alt>)
    ;; since the accessor assigns a value
    ;; when there is no alternative expression
    ;; the assumed syntax here is safe
    `(if ,(transform-sexp (if-predicate exp))
         ,(transform-sexp (if-consequent exp))
         ,(transform-sexp (if-alternative exp))))
   ((lambda? exp)
    ;; here we need to:
    ;; * scan exposed definitions
    ;; * eliminate them
    (error 'todo))
   ((begin? exp)
    ;; (begin <exp> ...)
    `(begin ,@(map
               transform-sexp
               (begin-actions exp))))
   ((cond? exp)
    ;; well, let's desugar it
    (transform-sexp (cond->if exp)))
   ((let? exp)
    ;; well, let's desugar it
    (transform-sexp (let->combination exp)))
   ((application? exp)
    ;; (<exp1> <exp2s> ...)
    `(,(transform-sexp (operator exp))
      ,@(map transform-sexp (operands exp))))
   (else
    (error "invalid s-expression: "
           exp))))

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
