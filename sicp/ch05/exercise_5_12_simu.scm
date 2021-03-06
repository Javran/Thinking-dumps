(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "simu.scm")

(load "exercise_5_12_analyze.scm")

(define (empty-machine)
  (vector
   (empty-stack)                        ; 0: stack
   '()                                  ; 1: instruction sequence
   '()                                  ; 2: register-table
   '()                                  ; 3: operations
   '()                                  ; 4: jump-table
   '()                                  ; 5: data-path-meta
   ))

(define (machine-intern-ref symbol)
  (case symbol
    ((stack)                0)
    ((instruction-sequence) 1)
    ((register-table)       2)
    ((operations)           3)
    ((jump-table)           4)
    ((data-path-meta)       5)
    (else (error "MACHINE: unknown internal ref: "
                 symbol))))

(define (machine-data-path-meta m)
  (vector-ref
   m
   (machine-intern-ref 'data-path-meta)))

(define (machine-set-data-path-meta! m new-meta)
  (machine-intern-set-field! m 'data-path-meta new-meta))


;; updated: now we don't consider the init-reg-table
(define (build-with
         controller-text
         init-reg-table
         ops-builder)
  (let* ((insns (cdr controller-text))
         (m (empty-machine))
         (reg-names (extract-register-names insns)))
    (machine-define-registers! m reg-names)
    (for-each
     (lambda (pair)
       (machine-reg-set! m (car pair) (cadr pair)))
     init-reg-table)
    (machine-set-operations! m (ops-builder m))
    ;; before assembling,
    ;; we analyze the instructions
    (machine-set-data-path-meta!
     m
     (data-path-analyze insns))
    (assemble insns m)
    m))

(define (machine-pretty-print-data-meta m)
  (pretty-print-data-path-analysis
   (machine-data-path-meta m)))

(load "figure_5_12.scm")

(let ((m (build-with
          fib-machine-controller
          ;; not necessary since we don't need to execute it
          ;; but anyway
          '((n 5))
          default-ops-builder)))
  (machine-pretty-print-data-meta m))
