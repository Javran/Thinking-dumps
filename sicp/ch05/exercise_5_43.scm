(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "compiler.scm")
(load "simu.scm")
(load "simu_compiler_patch.scm")

(load "exercise_5_43_common.scm")
(load "exercise_5_43_compiler.scm")

;; TODO: I think it's fine to always do the transformation before compiling
;; TODO: with all local definitions eliminated, we are now ready to
;;   apply the technique in ex 5.42.

;; test scan
(define (check-scan-consistency exp)
  (let* ((new-exp (transform-exp exp)))
    (equal? (eval exp user-initial-environment)
            (eval new-exp user-initial-environment))))

(assert
 (andf
  (map
   check-scan-consistency
   ;; test expressions
   ;; TODO: consider merging this for compiler tests
   ;; as well...
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
     ;; ====
     (begin
       (define f (lambda (x)
                   (lambda (y)
                     (+ x x y))))
       ((f 10) 20))
   ))))

;; TODO: the transformation itself works fine,
;; stuck in infinite loop when doing compiling
;; I guess forms like (let () <subexps>) is likely to be the culprit
;; a simple solution will be transform this into (begin <subexps>)
;; since local definitions are eliminated
;; but I hate to make special cases
;; let's find a way to do the transformation only once
;; TODO: will replacing "compile-and-run-with-env" or
;; "compile-and-verify" work?
(error "debug")

(load "ec-tests.scm")
(load "exercise_5_23_tests.scm")

;; testing the compiler
(for-each
 (test-evaluator
  compile-and-run-with-env)
 test-exps)

(end-script)
