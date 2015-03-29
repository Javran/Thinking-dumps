(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")
(load "compiler.scm")
(load "ec-plus.scm")

(compile-and-go
 `(begin
    ;; recursive factorial function
    ;;   compiled for collecting stack statistics
    (define (factorial n)
      (if (<= n 1)
          1
          (* n (factorial (- n 1)))))
    ))
