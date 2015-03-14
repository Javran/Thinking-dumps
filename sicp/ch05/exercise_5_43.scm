(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "exercise_5_43_common.scm")

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

(end-script)
