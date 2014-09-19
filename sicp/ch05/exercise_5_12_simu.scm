(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./simu.scm")

(load "./exercise_5_12_analyze.scm")

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

