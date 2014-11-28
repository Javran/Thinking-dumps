;; stop-and-copy garbage collector
;; some instructions are adapted to work with our machine
;; implementation

;; except for "free", "root", "new-cars", "the-cars", "new-cdrs" and "the-cdrs",
;; all registers are prefixed with a "gc-" to avoid name confliction with
;; the program
(define (gen-gc-code
         broken-heart-symbol)
  `(begin-garbage-collection
    ;; free: points to the first free memory address
    (assign free (op to-ptr) (const 0))
    ;; scan: used by gc-loop, points to the first shallow copy
    (assign gc-scan (op to-ptr) (const 0))
    ;; prepare to copy the first pair
    (assign gc-old (reg root))
    (assign gc-relocate-continue (label gc-reassign-root))
    (goto (label gc-relocate-old-result-in-new))

    ;; after the first pair is shallow-copied,
    ;; we update root and get started
    gc-reassign-root
    (assign root (reg gc-new))
    (goto (label gc-loop))

    gc-loop
    (test (op ptr=?) (reg gc-scan) (reg free))
    ;; scan == free means we have copied everything necessary
    ;; and it's time for swaping memories
    (branch (label gc-flip))
    ;; otherwise we still have some jobs to do.

    ;; relocate old pointers in `car` part
    (assign gc-old (op vector-ref) (reg new-cars) (reg gc-scan))
    (assign gc-relocate-continue (label gc-update-car))
    (goto (label gc-relocate-old-result-in-new))

    gc-update-car
    (perform (op vector-set!)
             (reg new-cars)
             (reg gc-scan)
             (reg gc-new))
    ;; relocate old pointers in `cdr` part
    (assign gc-old (op vector-ref) (reg new-cdrs) (reg gc-scan))
    (assign gc-relocate-continue (label gc-update-cdr))
    (goto (label gc-relocate-old-result-in-new))

    gc-update-cdr
    (perform (op vector-set!)
             (reg new-cdrs)
             (reg gc-scan)
             (reg gc-new))
    ;; scan next one
    (assign gc-scan (op ptr-inc) (reg gc-scan))
    (goto (label gc-loop))

    ;; this subroutine relocates the data pointed by `old` register
    ;; resulting in the corresponsing deep copy pointed by `new` register
    gc-relocate-old-result-in-new
    ;; note that when we are storing a label to the register
    ;; the actual code is stored there instead,
    ;; in practice we might just store a line number instead.
    ;; because obviously one cell is far from enough to store that much data
    ;; also note that the actual code is not something we can represent directly
    ;; using the machine instructions, but here we are just making a special case
    ;; for pairs and treating all the other values primitives,
    ;; so it's fine for registers like "continue" to store data onto the memory
    ;; since our GC is dealing with it properly
    (test (op pair?) (reg gc-old))
    (branch (label gc-pair))
    ;; if "old" does not contain a pair,
    ;; there's no ref in data, just copy it to the new one
    (assign gc-new (reg gc-old))
    (goto (reg gc-relocate-continue))

    gc-pair
    ;; test if we have found a broken-heart flag
    (assign gc-oldcr (op vector-ref) (reg the-cars) (reg gc-old))
    ;; for an already-moved structure, car is a broken-heart flag
    ;; and cdr is the new location (of this pair)
    (test (op broken-heart?) (reg gc-oldcr))
    (branch (label gc-already-moved))
    ;; if no broken-heart flag is found,
    ;; we need to relocate the old data.
    (assign gc-new (reg free))          ; new location for pair
    ;; update free pointer
    (assign free (op ptr-inc) (reg free))
    ;; copy the whole pair
    (perform (op vector-set!)
             (reg new-cars)
             (reg gc-new)
             (reg gc-oldcr))
    (assign gc-oldcr (op vector-ref) (reg the-cdrs) (reg gc-old))
    (perform (op vector-set!)
             (reg new-cdrs)
             (reg gc-new)
             (reg gc-oldcr))
    ;; construct the broken heart
    (perform (op vector-set!)
             (reg the-cars)
             (reg gc-old)
             (const ,broken-heart-symbol))
    (perform
     (op vector-set!) (reg the-cdrs) (reg gc-old) (reg gc-new))
    (goto (reg gc-relocate-continue))

    gc-already-moved
    ;; the operation "debug-print" is optional,
    ;; it just provides some evidence that we can reach this
    ;; case, and if the program can run without error and
    ;; the following debug message gets printed,
    ;; this part should be covered by the program
    (perform (op debug-print) (const "broken-heart flag found"))
    (assign gc-new (op vector-ref) (reg the-cdrs) (reg gc-old))
    (goto (reg gc-relocate-continue))

    ;; gc-flip swaps the working memories and the free memories
    gc-flip
    ;; the-cdrs <-> new-cdrs
    (assign gc-temp (reg the-cdrs))
    (assign the-cdrs (reg new-cdrs))
    (assign new-cdrs (reg gc-temp))
    ;; the-cars <-> new-cars
    (assign gc-temp (reg the-cars))
    (assign the-cars (reg new-cars))
    (assign new-cars (reg gc-temp))
    ))
