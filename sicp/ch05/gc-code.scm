;; TODO: adapt the code according to our implementation
;; TODO: design a special flag that distinguishes machine symbols
;; from scheme symbols so that a machine "broken-heart" symbol will never be
;; mistakenly understood as a real "broken-heard" flag by garbage collection
;; algorithm
;; TODO: initialize "root" register before gc starts
;; TODO: update registers according to "root" register
;; after the gc is done
;; TODO: try to do gc when a "cons" is about to use up the space

(define gc-code
  '(begin-garbage-collection
    ;; free: points to the first free memory address
    (assign free (op to-pointer) (const 0))
    ;; scan: used by gc-loop, points to the first shallow copy
    (assign scan (op to-pointer) (const 0))
    ;; prepare to copy the first pair
    (assign old (reg root))
    (assign relocate-continue (label reassign-root))
    (goto (label relocate-old-result-in-new))

    ;; after the first pair is shallow-copied,
    ;; we update root and get started
    reassign-root
    (assign root (reg new))
    (goto (label gc-loop))

    gc-loop
    ;; TODO: implement ptr-=
    (test (op =) (reg scan) (reg free))
    ;; scan == free means we have copied everything necessary
    ;; and it's time for swaping memories
    (branch (label gc-flip))
    ;; otherwise we still have some jobs to do.

    ;; relocate old pointers in `car` part
    (assign old (op vector-ref) (reg new-cars) (reg scan))
    (assign relocate-continue (label update-car))
    (goto (label relocate-old-result-in-new))

    update-car
    (perform (op vector-set!)
             (reg new-cars)
             (reg scan)
             (reg new))
    ;; relocate old pointers in `cdr` part
    (assign old (op vector-ref) (reg new-cdrs) (reg scan))
    (assign relocate-continue (label update-cdr))
    (goto (label relocate-old-result-in-new))

    update-cdr
    (perform (op vector-set!)
             (reg new-cdrs)
             (reg scan)
             (reg new))
    ;; scan next one
    (assign scan (op ptr-inc) (reg scan))
    (goto (label gc-loop))

    ;; this subroutine relocates the data pointed by `old` register
    ;; resulting in the corresponsing deep copy pointed by `new` register
    relocate-old-result-in-new
    (test (op pointer-to-pair?) (reg old))
    (branch (label pair))
    ;; if "old" does not contain a pair,
    ;; there's no ref in data, just copy it to the new one
    (assign new (reg old))
    (goto (reg relocate-continue))

    pair
    ;; test if we have found a broken-heart flag
    (assign oldcr (op vector-ref) (reg the-cars) (reg old))
    ;; for an already-moved structure, car is a broken-heart flag
    ;; and cdr is the new location (of this pair)
    (test (op broken-heart?) (reg oldcr))
    (branch (label already-moved))
    ;; if no broken-heart flag is found,
    ;; we need to relocate the old data.
    (assign new (reg free)) ; new location for pair
    ;; update free pointer
    (assign free (op ptr-inc) (reg free))
    ;; copy the whole pair
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
