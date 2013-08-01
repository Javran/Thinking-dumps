; test system
; given all the test cases, do test
; and raise error if the result is wrong

(define (do-test f testcases)
  (define (my-equal? x y)
    (cond ((equal? x y) #t)
          ((and (number? x) (number? y))
            (= x y))
          ((and (non-empty? x) (non-empty? y))
            (and (my-equal? (car x) (car y))
                 (my-equal? (cdr x) (cdr y))))
          (else #f)))

  (for-each
    (lambda (testcase)
      (let* ((args (car testcase))
             (expected (cdr testcase))
             (result (apply f args)))
        (if (my-equal? expected result)
          (display ".")
          (begin
            (out "Result:"
                 result
                 "Expected:"
                 expected)
            (error "Test failed.")))))
    testcases)
  (out "Test passed."))

(define (test-example)
  (let ((testcases (list
                     (cons (list 1 2 3) 6)
                     (cons (list 1 1) 2)
                     (cons (list -1 10) 9))))
    (do-test + testcases)))

; uncomment to see the example
; (test-example)

