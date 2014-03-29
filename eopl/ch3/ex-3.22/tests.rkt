(module tests mzscheme
  
  (provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
  
  (define test-list
    '(
  
      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "(diff 44 33)" 11)
  
      ;; nested arithmetic
      (nested-arith-left "(diff (diff 44 33) 22)" -11)
      (nested-arith-right "(diff 55 (diff 22 11))" 44)
  
      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "(diff x 1)" 9)
      (test-var-3 "(diff 1 x)" -9)
      
      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "(diff x foo)" error)
  
      ;; simple conditionals
      (if-true "if (zero? 0) then 3 else 4" 3)
      (if-false "if (zero? 1) then 3 else 4" 4)
      
      ;; test dynamic typechecking
      (no-bool-to-diff-1 "(diff (zero? 0) 1)" error)
      (no-bool-to-diff-2 "(diff 1 (zero? 0))" error)
      (no-int-to-if "if 1 then 2 else 3" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly. 
      (if-eval-test-true "if (zero? (diff 11 11)) then 3 else 4" 3)
      (if-eval-test-false "if (zero? (diff 11 12)) then 3 else 4" 4)
      
      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "if (zero? (diff 11 11)) then 3 else foo" 3)
      (if-eval-test-false-2 "if (zero? (diff 11 12)) then foo else 4" 4)

      ;; simple let
      (simple-let-1 "let x = 3 in x" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "let x = 3 in (diff x 1)" 2)
      (eval-let-rhs "let x = (diff 4 1) in (diff x 1)" 2)

      ;; check nested let and shadowing
      (simple-nested-let "let x = 3 in let y = 4 in (diff x y)" -1)
      (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
      (check-shadowing-in-rhs "let x = 3 in let x = (diff x 1) in x" 2)

      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x) (diff x 1)  30)" 29)
      (apply-simple-proc "let f = proc (x) (diff x 1) in (f 30)" 29)
      (let-to-proc-1 "(proc(f)(f 30)  proc(x)(diff x 1))" 29)


      (nested-procs "((proc (x) proc (y) (diff x y)  5) 6)" -1)
      (nested-procs2 "let f = proc(x) proc (y) (diff x y) in ((f (diff 10 5)) 6)"
        -1)
      
      (y-combinator-1 "
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if (zero? x) then 0 else (diff (f (diff x 1)) -4)
in let times4 = (fix t4m)
   in (times4 3)" 12)

      ;; multiple arg supports
      (multi-args-proc-1 "(proc (a,b,c) (diff c (diff a b)) 10 20 30)" 40)
      (multi-args-proc-2 "let f = proc (x,conseq,alter)
                                    if (zero? x)
                                      then conseq
                                      else alter
                          in (f 0 1 2)" 1)
      (multi-args-proc-3 "let f = proc (x,y,conseq,alter)
                                    if (zero? (diff x y))
                                      then conseq
                                      else alter
                          in (f 0 1 1 2)" 2)
      (multi-args-proc-4 "(proc (x,y) (diff x y) 1)" error)
      (multi-args-proc-5 "(proc (x) x 1 1)" error)

      ))
  )
