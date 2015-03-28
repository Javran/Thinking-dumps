;;; working tests for ec-plus
(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

(compile-and-go
 ;; try putting some definitions here.
 ;; this part of expression is compiled
 ;; and loaded into the machine
 ;; when it starts executing.
 ;;
 ;; the difference is:
 ;; when you start doing interaction with the evaluator
 ;; your expresssion are interpreted directly
 ;; while the expression here is compiled thus has less overhead
 ;; than its interpreted version
 ;; (simu-monitor-patch.scm is installed by default
 ;; so that you can see number of pushes
 ;; and the max depth of stack directly)
 `(begin
    (define (product xs)
      (if (null? xs)
          1
          (* (car xs)
             (product (cdr xs)))))
    (define (sum xs)
      (define (sum-intern acc ys)
        (if (null? ys)
            acc
            (sum-intern (+ acc (car ys))
                        (cdr ys))))
      (sum-intern 0 xs))))
