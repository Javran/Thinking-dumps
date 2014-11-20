;; TODO: to test the functionality
;; of garbage collection, we use a program
;; that would take up enough space to trigger
;; the garbage collection routine and see
;; if the result is still correct or not.

;; some settings:
;; * define memory to have only 256 cells.
;; * generate a list [1..100]
;; * apply "map (* 2)" to it 4 times (this will create many garbages
;;   and hopefully trigger the garbage collection)
;; * take sum of it and print out the result
;; * sum . map (* 2) . map (* 2) . map (* 2) . map (* 2) $ [1..100] = 80800

(define test-machine
  `(controller
    (goto (label prog-entry))
    gen-list
    ;; generate a list: [a .. b]
    ;; input registers are "a" and "b"
    (test (op <=) (reg a) (reg b))
    (branch (label gen-list-next-value))
    ;; case: a > b
    (assign result (const ()))
    (goto (reg continue))
    ;; case: a <= b
    gen-list-next-value
    (save continue)                     ; stack: [continue ..]
    (save a)                            ; stack: [a continue ..]
    (assign continue (label gen-list-after-sublist))
    (assign a (op +) (reg a) (const 1))
    (goto (label gen-list))
    ;; recursively, construct [a+1 .. b]
    gen-list-after-sublist
    (restore a)                         ; stack: [continue ..]
    (assign result (op cons) (reg a) (reg result))
    (restore continue)                  ; stack: <balanced>
    (goto (reg continue))

    sum-of-list
    ;; input: a list "a"
    (test (op null?) (reg a))
    (branch (label sum-of-empty-list))
    (assign tmp (op car) (reg a))
    (save continue)                     ; stack: [continue ..]
    (save tmp)                          ; stack: [tmp continue ..]
    (assign a (op cdr) (reg a))
    (assign continue (label sum-of-list-after-sublist))
    (goto (label sum-of-list))
    sum-of-list-after-sublist
    (restore tmp)                       ; stack: [continue ..]
    (assign result (op +) (reg result) (reg tmp))
    (restore continue)                  ; stack: <balanced>
    (goto (reg continue))

    sum-of-empty-list
    (assign result (const 0))
    (goto (reg continue))

    double-list
    ;; input: reg a
    ;; output: reg result
    (test (op null?) (reg a))
    (branch (label double-list-empty))
    ;; non-empty-list
    (assign tmp (op car) (reg a))
    (assign tmp (op *) (reg tmp) (const 2))
    (save continue)                     ; stack: [continue ..]
    (save tmp)                          ; stack: [tmp continue ..]
    (assign a (op cdr) (reg a))
    (assign continue (label double-list-after-sublist))
    (goto (label double-list))
    double-list-after-sublist
    (restore tmp)                       ; stack: [continue ..]
    (assign result (op cons) (reg tmp) (reg result))
    (restore continue)                  ; stack: <balanced>
    (goto (reg continue))

    ;; empty list
    double-list-empty
    (assign result (const ()))
    (goto (reg continue))

    prog-entry
    ;; generate [1..100]
    (assign a (const 1))
    (assign b (const 100))
    (assign continue (label prog-after-gen-list))
    (goto (label gen-list))
    prog-after-gen-list
    ;; double it 4 times
    (assign b (const 4))
    prog-double-loop
    (test (op =) (reg b) (const 0))
    (branch (label prog-double-done))
    ;; if the counter "b" has not yet reached "0"
    (assign b (op -) (reg b) (const 1))
    (save b) ; stack: [b ..]
    (assign a (reg result))
    (assign continue (label prog-after-double))
    (goto (label double-list))
    prog-after-double
    (restore b) ; stack: <balanced>
    (goto (label prog-double-loop))

    prog-double-done
    (assign a (reg result))
    (assign continue (label prog-end))
    (goto (label sum-of-list))
    prog-end
    ))

(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")
(load "./simu_lower_patch.scm")

(let ((m (build-and-execute
          test-machine
          '())))
  (out (machine-reg-get m 'result)
       "free:"
       (machine-reg-get m 'free)))
