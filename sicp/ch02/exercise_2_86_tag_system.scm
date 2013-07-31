; tag system
(load "./4_3_data_directed_put_get.scm")

; overwrite `apply-generic`
(load "./exercise_2_86_apply_generic.scm")

; after applying args to procedure `f`
; attach a tag `tag` on the result
(define (tagged tag f)
  (lambda args
    (attach-tag tag
                (apply f args))))

(define (test-tagged)
  (let ((f (tagged 'test-tag +))
        (testcases (list
                     (cons (list 1 2 3)
                           (cons 'test-tag 6))
                     (cons (list -1 1)
                           (cons 'test-tag 0)))))
    (do-test f testcases)))

; uncomment to see the test results
; (test-tagged)
