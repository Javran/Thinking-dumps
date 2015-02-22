(load "simu_lower_patch.scm")
(load "gc-transform.scm")

;; we use "gensym" to generate the broken-heart symbol
;; before we execute the machine.
;; Therefore the broken heart symbol is guaranteed to be unique
;; We ensure the uniqueness of this symbol so the garbage collecting
;; algorithm will not think a symbol constant happens to be a broken heart flag
(define machine-gc-broken-heart
  (gensym))

(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      `((broken-heart? ,(lambda (sym)
                          (eq? sym machine-gc-broken-heart)))
        (debug-gc-start ,(lambda ()
                           (out "GC triggered")))
        (debug-gc-end ,(lambda ()
                         (format
                          #t
                          "GC done (~A/~A live cells)~%"
                          (machine-pointer-get
                           (machine-reg-get m 'free))
                          machine-memory-size)))
        ,@(del-assoc 'initialize-stack
                     (old-builder m))))))

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
  (gc-transform-program-with
   machine-gc-broken-heart
   machine-memory-size))
