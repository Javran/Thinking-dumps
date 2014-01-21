(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "-(44,33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "-(-(44,33),22)" -11)
      (nested-arith-right "-(55, -(22,11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "-(x,1)" 9)
      (test-var-3 "-(1,x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "-(x,foo)" error)
  
      ;; simple conditionals
      (if-true "if zero?(0) then 3 else 4" 3)
      (if-false "if zero?(1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "-(zero?(0),1)" error)
      (no-bool-to-diff-2 "-(1,zero?(0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
      (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in -(x,1)" 2)
      (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)

      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
      (apply-simple-proc "let f = proc (x) -(x,1) in (f 30)" 29)
      (let-to-proc-1 "(proc(f)(f 30)  proc(x)-(x,1))" 29)


      ;; note that dynamic binding is used. in the initial env, x = 10
      ;; currying does not work as expected, because the value of x is not passed into
      ;; the nested procedure. here x = 10, y = 6, result = 4
      (nested-procs "((proc (x) proc (y) -(x,y)  5) 6)" 4)
      ;; to verify, if we change the variable name, it should raise an error
      (nested-procs2 "((proc (a) proc (b) -(a,b)  5) 6)" error)
      ;; same problem, x is bound to 10 no matter how the environment is changed
      ;; the result of `-(10,5)` is dropped, and `-(x,6)` yields 4
      (nested-procs3 "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)" 4)

      (book-example "let a = 3 in let p = proc (x) -(x,a) in let a = 5 in -(a,(p 2))" 8)

      (ex-3-29-example-1 "let a = 3 in let p = proc (z) a in let f = proc (x) (p 0) in let a = 5 in (f 2)" 5)
      (ex-3-29-example-2 "let a = 3 in let p = proc (z) a in let f = proc (a) (p 0) in let a = 5 in (f 2)" 2)

      (add1-test "add1(10)" 11)
      (mul-test "*(10,11)" 110)

      (recursive-test "
        let fact = proc (n) add1(n)
        in let fact = proc (n)
                        if zero?(n)
                          then 1
                          else *(n,(fact -(n,1)))
        in (fact 5)" 120)

      (mutual-recursive-test-1 "
        let even = proc (x)
                     if zero?(x)
                       then 1
                       else (odd -(x,1))
        in let odd = proc (x)
                       if zero?(x)
                         then 0
                         else (even -(x,1))
        in (odd 13) " 1)
      (mutual-recursive-test-2 "
        let even = proc (x)
                     if zero?(x)
                       then 1
                       else (odd -(x,1))
        in let odd = proc (x)
                       if zero?(x)
                         then 0
                         else (even -(x,1))
        in (odd 12) " 0)
      (mutual-recursive-test-3 "
        let even = proc (x)
                     if zero?(x)
                       then 1
                       else (odd -(x,1))
        in let odd = proc (x)
                       if zero?(x)
                         then 0
                         else (even -(x,1))
        in (even 13) " 0)
      ))
  )
