(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu_utils.scm")
(load "ec-prim.scm")
(load "exercise_5_23_common.scm")
(load "set.scm")

(load "exercise_5_43_scan.scm")
(load "exercise_5_43_transform.scm")

;; test scan
(define (check-scan-consistency exp)
  (let* ((scan-result (scan-definitions-and-transform exp))
         (binding-set (car scan-result))
         (transformed-exp (cdr scan-result))
         (new-exp `(let ,(map (lambda (var)
                                `(,var '*unassigned*))
                              binding-set)
                     ,transformed-exp)))
    (pretty-print new-exp)
    (equal? (eval exp user-initial-environment)
            (eval new-exp user-initial-environment))))

(out (check-scan-consistency
      '(begin
         (define x 10)
         (define y 20)
         (define (f x y)
           (* x y))
         (+ x y (f x y)))))

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

;; TODO: scan-and-transform approach needs 2 traversals
;; but I think only one is necessary
;; before we try to do this traversal-fusion,
;; let's first have a correct implementation

;; TODO: need some unit tests to figure it out..
#;(pretty-print
 (transform-sexp
  `(lambda (x)
     (begin
       (define y 1)
       (define z 2)
       (if (= x 0)
           (begin
             (define a 10)
             (+ x a))
           (begin
             (define b 320)
             ;; TODO: local function definition results
             ;; in infinite loop...
             (define (f x y)
               (+ x y))
             (f (* x 3 b) 20)))))))


(end-script)
