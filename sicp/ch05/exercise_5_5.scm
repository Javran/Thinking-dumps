(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (controller-description? c)
  (and (list? c)
       (not (null? c))
       (eq? 'controller (car c))))

;; instead of hand-simulation, let's try to write one simulator
(define (run-machine controller-desc)
  (let ((instructions (cdr controller-desc)))
    (let ((label-insn-alist
           (let loop ((insns instructions)
                      (lbl-insn-alist '()))
             (if (null? insns)
                 lbl-insn-alist
                 (if (symbol? (car insns))
                     ;; add a new label
                     (loop
                      (cdr insns)
                      (cons (list (car insns) (cdr insns))
                            lbl-insn-alist))
                     ;; skip it
                     (loop
                      (cdr insns)
                      lbl-insn-alist))))))
      (out label-insn-alist))))

(run-machine
 '(controller
   label-1
   (assign n (reg aaa))
   label-2
   (assign n (reg bbb))
   label-3
   label-4))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
