(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

(load "./natural_language_common.scm")

(for-each
 (lambda (e)
   (pretty-print e) (newline) (newline))
 (stream-take
  5
  (amb-eval-stream
   (run-source-in-env
    `(begin
       (define (parse-word word-list)
         (list (car word-list)
               (an-element-of (cdr word-list))))
       (parse '())
       ))
   (amb-init-env))))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
