(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; this should be the main file.
;; TODO list:
;; * data directed dispatch
;;   (use my handler-register style instead,
;;    which also enables some unit tests)

(define (find-assertions pattern frame)
  (stream-flatmap
   (lambda (datum)
     (check-an-assertion datum pattern frame))
   (fetch-assertions pattern frame)))

(define (check-an-assertion
         assertion
         query-pat
         query-frame)
  (let ((match-result
         (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

;; Local variables:
;; proc-entry: ""
;; End:
