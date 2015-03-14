;; export functionalities

(load "set.scm")
(load "ec-prim.scm")
(load "simu_utils.scm")
(load "exercise_5_23_common.scm")

(load "exercise_5_43_scan.scm")
(load "exercise_5_43_transform.scm")

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
