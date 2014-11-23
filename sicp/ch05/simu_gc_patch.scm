(load "./simu_lower_patch.scm")
(load "./gc-code.scm")
(load "./gc-transform.scm")

;; registers that shouldn't be stored by "root" register
(define machine-reserved-gc-registers
  (remove-duplicates
   `(gc-resume-point
     gc-flag
     ,@(extract-register-names gc-code))))

(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      `((broken-heart? ,(lambda (sym)
                          (eq? sym gc-broken-heart)))
        (debug-gc-start ,(lambda ()
                           (out "GC triggered")))
        (debug-gc-end ,(lambda ()
                         (format
                          #t
                          "GC done (~A/~A live cells)~%"
                          (machine-pointer-get
                           (machine-reg-get m 'free))
                          machine-memory-size)))
        ,@(old-builder m)))))

(define machine-memory-size 512)

(define (machine-fresh-start! m)
  ;; initialize two pieces of memories
  (machine-reg-set! m 'free (machine-pointer 0))
  ;; we now needs 4 registers to store memories of the same size
  ;; so that we can flip them alternatively.
  (machine-reg-set! m 'the-cars (make-vector machine-memory-size))
  (machine-reg-set! m 'the-cdrs (make-vector machine-memory-size))
  (machine-reg-set! m 'new-cars (make-vector machine-memory-size))
  (machine-reg-set! m 'new-cdrs (make-vector machine-memory-size))
  (machine-reg-set! m 'the-stack '())
  (machine-reset-pc! m)
  (machine-execute! m))

;; user should avoid using any register whose name is prefixed with "gc-"
(define machine-do-insn-list-preprocess
  gc-transform-program)
