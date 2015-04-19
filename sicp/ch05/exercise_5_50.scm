(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define metacircular-program
  (call-with-input-file
      "metacircular-evaluator.scm"
    (lambda (p)
      (let loop ((next-result (read p)))
        (if (eof-object? next-result)
            '()
            (cons next-result (loop (read p))))))))

(pretty-print
 `(begin
    ,@metacircular-program))

;; (load "compiler.scm")

#;(compile-and-check
 `(begin
    ,@metacircular-program))


(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
