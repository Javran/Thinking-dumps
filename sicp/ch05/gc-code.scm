(define gc-code
  '(begin-garbage-collection
    (assign free (const 0))
    (assign scan (const 0))
    (assign old (reg root))
    (assign relocate-continue (label reassign-root))
    (goto (label relocate-old-result-in-new))

    reassign-root
    (assign root (reg new))
    (goto (label gc-loop))

    gc-loop
    (test (op =) (reg scan) (reg free))
    ;; scan == free means we have copied everything necessary
    ;; time to swap memories
    (branch (label gc-flip))
    (assign old (op vector-ref) (reg new-cars) (reg scan))
    (assign relocate-continue (label update-car))
    (goto (label relocate-old-result-in-new))

    update-car
    (perform (op vector-set!)
             (reg new-cars)
             (reg scan)
             (reg new))
    (assign old (op vector-ref) (reg new-cdrs) (reg scan))
    (assign relocate-continue (label update-cdr))
    (goto (label relocate-old-result-in-new))

    update-cdr
    (perform (op vector-set!)
             (reg new-cdrs)
             (reg scan)
             (reg new))
    (assign scan (op +) (reg scan) (const 1))
    (goto (label gc-loop))

    relocate-old-result-in-new
    (test (op pointer-to-pair?) (reg old))
    (branch (label pair))
    (assign new (reg old))
    (goto (reg relocate-continue))

    pair
    (assign oldcr (op vector-ref) (reg the-cars) (reg old))
    (test (op broken-heart?) (reg oldcr))
    (branch (label already-moved))
    (assign new (reg free)) ; new location for pair
    ;; update free pointer
    (assign free (op +) (reg free) (const 1))
    (perform (op vector-set!)
             (reg new-cars)
             (reg new)
             (reg oldcr))
    (assign oldcr (op vector-ref) (reg the-cdrs) (reg old))
    (perform (op vector-set!)
             (reg new-cdrs)
             (reg new)
             (reg oldcr))
    ;; construct the broken heart
    (perform (op vector-set!)
             (reg the-cars)
             (reg old)
             (const broken-heart))
    (perform
     (op vector-set!) (reg the-cdrs) (reg old) (reg new))
    (goto (reg relocate-continue))

    already-moved
    (assign new (op vector-ref) (reg the-cdrs) (reg old))
    (goto (reg relocate-continue))

    ;; gc-flip swaps the working memories and the free memories
    gc-flip
    ;; the-cdrs <-> new-cdrs
    (assign temp (reg the-cdrs))
    (assign the-cdrs (reg new-cdrs))
    (assign new-cdrs (reg temp))
    ;; the-cars <-> new-cars
    (assign temp (reg the-cars))
    (assign the-cars (reg new-cars))
    (assign new-cars (reg temp))

    ))
