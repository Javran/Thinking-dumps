; simple data types, like number, string, etc.

; recognize simple expressions
;   that evaluates to itself
(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)
      (char? exp)
      (boolean? exp)))

; variables are just symbols
(define variable? symbol?)

(define (test-eval-simple)
  (let ((testcases
          (list
            (mat 'abc #f)
            (mat 1    #t)
            (mat 1.5  #t)
            (mat '(1 2 3) #f)
            (mat "a"  #t)
            (mat #\a  #t))))
    (do-test self-evaluating? testcases))
  (let ((testcases
          (list
            (mat 'a   #t)
            (mat '(a) #f)
            (mat 10   #f))))
    (do-test variable? testcases))
  'ok)

(if *my-eval-do-test*
  (test-eval-simple))
