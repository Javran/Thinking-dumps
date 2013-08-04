; require "./utils.scm"

(define (assert result . reason)
  (if result
    'ok
    (if (null? reason)
      (error "assertion failed.")
      (error "assertion failed:" (car reason)))))

(define (do-test-ex proc testcases correct? on-correct on-wrong)
  ; proc:
  ;   procedure to be tested
  ; testcases:
  ;   a list of testcase,
  ;     a testcase is constructed by (cons <args> <expected>)
  ;     where <args> is either a list of arguments or 
  ;     a non-list object(for unary procedures)
  ;     and <expected> is the expected result
  ;   e.g.: a valid testcase to test `+` might be:
  ;     (list (cons (list 1 2 3) 6)
  ;           (cons (list 4 5 6) 15))
  ; correct?
  ;   accepts two arguments that one comes from the result of `proc`
  ;   and another from <expected>, should return #t when a test case is passed
  ; on-correct
  ;   when a testcase passed, on-correct will be called 
  ;   with the corresponding testcase
  ; on-wrong
  ;   when a testcase failed, on-wrong will be called
  ;   with the corresponding testcase and the actual result
  (define (make-sure-list x)
    (if (list? x) x (list x)))

  (define (test-single-case testcase)
    (let* (; make sure the argument list is a list
           (input (make-sure-list (car testcase)))
           (expected (cdr testcase))
           (result (apply proc input)))
      (if (correct? result expected)
        (on-correct testcase)
        (on-wrong testcase result))))

  (for-each test-single-case testcases))
