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

;; from the result we could see that the first few sentences are:
;; * the student studies for the student
;; * the student studies for the student for the student ...
;; which is uninteresting. This is because the recursion get stuck
;; on one of the recursively defined structures and cannot get out
;; to try the next, more interesting alternatives.
;; Even if there are listed as a possible candicate, they don't
;; get a chance to appear

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
