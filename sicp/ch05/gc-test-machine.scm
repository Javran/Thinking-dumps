;; to test the functionality
;; of garbage collection, we use a program
;; that would take up enough space to trigger
;; the garbage collection routine and see
;; if the result is still correct or not.

;; some settings:
;; * each memory has only 512 cells, but in the program
;;   we traverse the list multiple times, each time a new
;;   list will be generated with each element multipled by 2
;;   this will hopefully use up more than 512 cells and trigger
;;   garbage collection.
;; * generate a list [1..100]
;; * apply "map (* 2)" to it 4 times (this will create many garbages
;;   and hopefully trigger the garbage collection)
;; * take sum of it and print out the result
;; * sum . map (* 2) . map (* 2) . map (* 2) . map (* 2) $ [1..100] = 80800
;; * TODO: now we are calling "map (* 2)" 8 times to see gc in action!

;; originally we use list [1..100] as input and 256 as memory size, and "double-list"
;; is only performed 4 times. But using 256 cells turns out to be insufficient
;; and garbage collection founds out that all the cells were alive. Therefore
;; we double the memory size and everything works fine.
;; I guess the number peak for living cells comes when
;; we are duplicating the list - both the original one and the new one
;; needs to be kept, plus that we are storing stack onto the memory as well.
;; and this might explain why 256 cells is still insufficient when dealing with
;; a list of size 100 (actually by trial and error, we found out that
;; memory size should be greater or equal to 308, 3 times the original list size,
;; which agrees with our guess (original list+ new list + stack))

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
    (assign b (const 128))
    (assign continue (label prog-after-gen-list))
    (goto (label gen-list))
    prog-after-gen-list
    ;; double it 8 times
    (assign b (const 8))
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
(load "./simu_gc_patch.scm")

;; TODO: mutate pairs to test broken-heart flag
;; TODO: list size was ~100 but took up ~200 mem space??
(let ((m (build-and-execute
          test-machine
          '())))
  ;; sum . map (* 2^8) $ [1..100] = 1292800
  ;; TODO: [1..128] looks more interesting
  (out (machine-reg-get m 'result)))
