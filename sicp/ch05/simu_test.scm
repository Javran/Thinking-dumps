(load "simu_execute.scm")

;; input a list of instructions, and expected register values,
;; raise error if the actual register value is not equal to the expected value
(define (do-machine-test insns result-regs)
  ;; since registers are unassigned at the begining,
  ;; to get a full list of registers we only need to take care
  ;; about "assign" instructions

  ;; result-regs: (list (list <reg-name> <reg-value>) ...)
  (let* ((m (build-and-execute-with
             ;; convert instruction list to controller text
             (cons 'controller
                   insns)
             ;; initial register values (optional)
             '()
             ;; opreation table
             (lambda (m)
               `(;; using all known default operations
                 ,@(default-ops-builder m)
                 ;; first instruction from a pc-like register
                 (first-insn ,caar)
                 ;; "perform test", assign value to register "a"
                 (perf ,(lambda (val)
                          (machine-reg-set! m 'a val)))
                 )))))
    (let ((testcases
           (map
            (lambda (result-reg-info)
              (mat m (car result-reg-info) (cadr result-reg-info)))
            result-regs)))
      (do-test
       machine-reg-get
       testcases))))

(define (do-handler-tests)
  ;; ==== test "assign" instruction ====
  ;; (const <val>)
  (do-machine-test
   '((assign a (const 1))
     (assign b (const 2))
     (assign b (const 3)))
   '((a 1)
     (b 3)))

  ;; (op <op>) and (reg <reg>)
  (do-machine-test
   '((assign a (const 1))
     (assign b (op +) (const 2) (reg a))
     (assign c (reg b)))
   '((a 1)
     (b 3)
     (c 3)))

  ;; this testcase is for test purpose only,
  ;; assigning to pc register is discouraged
  ;; because it might lead to confusions
  ;; (label <label>)
  (do-machine-test
   '((assign a (const 1))
     (assign pc (label jmp))
     (assign a (op +) (reg a) (const 10))
     jmp
     ;; the instruction right after the label
     ;; is skipped since "assign" always advances pc
     (assign a (op +) (reg a) (const 100))
     (assign a (op +) (reg a) (const 1000)))
   '((a 1001)))

  ;; ==== test "test" instruction ====
  ;; #f
  (do-machine-test
   '((test (op zero?) (const 1)))
   '((flag #f)))

  ;; #t, and with register access
  (do-machine-test
   '((assign a (const 0))
     (test (op zero?) (reg a)))
   '((flag #t)))

  ;; ==== test "branch" instruction ====
  ;; jump with #t flag
  (do-machine-test
   '((test (op zero?) (const 0))
     (assign a (const 1))
     (branch (label label-1))
     (assign a (op +) (reg a) (const 10))
     label-1
     (assign a (op +) (reg a) (const 100)))
   '((a 101)))

  ;; don't jump with #f flag
  (do-machine-test
   '((test (op zero?) (const 1))
     (assign a (const 1))
     (branch (label label-1))
     (assign a (op +) (reg a) (const 10))
     label-1
     (assign a (op +) (reg a) (const 100)))
   '((a 111)))

  ;; ==== test "goto" instruction ====
  ;; go to a label
  (do-machine-test
   '((assign a (const 1))
     (goto (label skip))
     (assign a (op +) (reg a) (const 10))
     skip
     (assign a (op +) (reg a) (const 100))
     (assign a (op +) (reg a) (const 1000)))
   '((a 1101)))

  ;; go to a register
  (do-machine-test
   '((assign a (const 10))
     (assign b (const 1))
     ;; label "pcx" should be here
     (assign pcx (reg pc))
     (test (op zero?) (reg a))
     (branch (label end))
     (assign a (op -) (reg a) (const 1))
     (assign b (op +) (reg b) (reg b))
     (goto (reg pcx))
     end
     (assign pcx (op first-insn) (reg pcx)))
   '((a 0)
     (b 1024)
     (pcx (assign pcx (reg pc)))))

  ;; ==== test "save" and "restore" instructions ====
  (do-machine-test
   '((assign a (const 1))
     (assign b (const 2))
     (assign c (const 3))
     (save a) ;; [a]
     (save b) ;; [b,a]
     (save c) ;; [c,b,a]
     (restore a)  ;; [b,a], new a = c = 3
     (restore c)  ;; [a], new c = b = 2
     (restore b)) ;; [], new b = a = 1
   '((a 3)
     (b 1)
     (c 2)))

  ;; ==== test "perform" instruction ====
  ;; trigger a "perform" operation
  (do-machine-test
   '((assign a (const 1))
     (perform (op perf) (const 6174)))
   '((a 6174)))

  ;; === test if "extract-register-names" works on "restore"
  (do-machine-test
   '((assign a (const 1))
     (save a)
     (restore b))
   '((a 1) (b 1)))

  'done)

(define (test-multiple-same-label)
  (assert-error
   (lambda ()
     (let ((m (build-and-execute
               `(controller
                 label-a
                 (assign n (const 1))
                 label-a
                 (assign n (const 2))
                 label-a
                 (assign n (const 3)))
               '((n 10)))))
       (out (machine-reg-get m 'n))))
   "mutiple labels with the same name will raise an error"))

(define (test-extra-slot)
  (let ((m (empty-machine)))
    (machine-extra-set! m 'a 10)
    (machine-extra-set! m 'b 20)
    (assert (= 20 (machine-extra-get m 'b 1)))
    (assert (= 10 (machine-extra-get m 'a 1)))

    (machine-extra-set! m 'a 'good)
    (assert (eq? 'good (machine-extra-get m 'a 1)))

    (machine-extra-modify! m 'b add1 1)
    (assert (= 21 (machine-extra-get m 'b 1)))

    (assert (= 10 (machine-extra-get m 'no-such-a-var 10)))
    (machine-extra-modify! m 'xxxx add1 10)
    (assert (= 11 (machine-extra-get m 'xxxx #f)))

    'done))

(if *simu-test*
    (begin
      (test-first-dup-element)
      (do-handler-tests)
      (test-multiple-same-label)
      (test-extra-slot))
    'skipped)

;; Local variables:
;; proc-entry: "./simu.scm"
;; End:
