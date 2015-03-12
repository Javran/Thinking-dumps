(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu_utils.scm")
(load "ec-prim.scm")
(load "exercise_5_23_common.scm")
(load "set.scm")

(load "exercise_5_43_scan.scm")
(load "exercise_5_43_transform.scm")

;; TODO: function names are confusing
(define (make-exp-from-scan-result scan-result)
  (let* ((binding-set (car scan-result))
         (transformed-exp (cdr scan-result)))
    `(let ,(map (lambda (var)
                  `(,var '*unassigned*))
                binding-set)
       ,transformed-exp)))

;; test scan
(define (check-scan-consistency exp)
  (let* ((new-exp (make-exp-from-scan-result
                   (scan-and-transform-exp exp))))
    (equal? (eval exp user-initial-environment)
            (eval new-exp user-initial-environment))))

;; "and" as a function
;; note that since "andf" is a normal function
;; its variable will be evaluated *before* entering
;; the function body.
;; therefore:
;; (and #f (error)) is fine, but
;; (andf #f (error)) is not.
(define (andf . args)
  (fold-left (lambda (a b)
               (and a b))
             #t
             args))

(assert
 (andf
  (map
   check-scan-consistency
   ;; test expressions
   '(1
     'a
     (let ()
       1)
     (let ()
       (define x 10)
       (define y 20)
       (define k (+ 2 x))
       (+ k x y))
     ;; test for coverage
     (begin
       ;; definition/variable
       (define x 10)
       ;; variable
       x
       ;; begin
       (begin
         (define y
           (if #t
               20
               40))
         ;; assignment
         (set! y 30))
       ;; definition/function
       (define (f u . vs)
         (* u (apply + vs)))
       ;; explicit lambda
       (define t (lambda (x) x))
       ;; application
       (+ (t x)
          ;; cond
          (cond (#f (t y))
                (else (t (t y))))
          ;; let
          (let ()
            (define k (+ 2 x))
            (+ k (f x y y y x)))))
     ;; ====
     ((lambda ()
        (define x 1)
        (define y 2)
        (+ x y)))
     ;; ====
     ((lambda (x)
        (begin
          (define y 1)
          (define z 2)
          (if (= x 0)
              (let ()
                (define a 10)
                (+ x a))
              (let ()
                (define b 320)
                (* x 3 b)))))
      1234)
   ))))

;; TODO: scan-and-transform approach needs 2 traversals
;; but I think only one is necessary
;; before we try to do this traversal-fusion,
;; let's first have a correct implementation

(end-script)
