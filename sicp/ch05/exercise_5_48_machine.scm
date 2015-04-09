(define (ec-ops-builder-modifier current-ops-builder)
  (lambda (m)
    (let* ((old-ops
            ;; step #1: passing the machine to generate
            ;; a list of operations
            (current-ops-builder m))
           (current-ops
            ;; step #2: adding some more (these primitives
            ;; usually need to know the machine object,
            ;; thus cannot be lifted from scheme)
            `((get-global-environment
               ,(lambda ()
                  (machine-extra-get m 'global-env 'error)))
              (error ,(lambda args
                        (apply error args)))
              ;; let's call this new primitive operation
              ;; "magic-compile" -- why not :)
              ;; "magic-compile" compiles the expression.
              ;; assemble the compiled code
              ;; and returns the assembled instructions
              (magic-compile
               ,(lambda (exp)
                  (let* ((compiled (compile exp 'val 'return))
                         (insn-seq (statements compiled))
                         (entry (assemble insn-seq m)))
                    ;; put instruction sequence in "val"
                    ;; and we are done
                    entry)))
              ,@old-ops))
           (missing-prim-symbols
            ;; step #3: find the list of missing operations
            ;; and lift them from scheme
            (set-diff
             (ec-get-required-operations)
             (remove-duplicates
              (map car current-ops)))))
      `(,@(map to-machine-prim-entry missing-prim-symbols)
        ,@current-ops))))
