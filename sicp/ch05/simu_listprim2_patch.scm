;; the real implementation
;; of list primitives
;; I guess this will eventually replace the ad hoc
;; implementation of "listprim"

;; usually we rewrite an instruction
;; into a sequence of instructions that does
;; some lower level operations
;; so here a single rule consists of a pattern matching
;; on an instruction and rewriting rule to rewrite it into
;; *a list* of instructions.

(define list-primitives-rules
  `( (;; rewrite `car`
      (assign $reg1 (op car) (reg $reg2))
      ;;
      ((assign $reg1 (op vector-ref) (reg the-cars) (reg $reg2))
       ))
     (;; rewrite `cdr`
      (assign $reg1 (op cdr) (reg $reg2))
      ;;
      ((assign $reg1 (op vector-ref) (reg the-cdrs) (reg $reg2))
       ))
     (;; rewrite `set-car!`
      (perform (op set-car!) (reg $reg1) $value)
      ;; instead of capturing `(reg $reg2)`, we capture the whole value
      ;; this would allow settings the `car` part of a pair to some constants
      ((perform (op vector-set!) (reg the-cars) (reg $reg1) $value)
       ))
     (;; rewrite `set-cdr!`
      (perform (op set-cdr!) (reg $reg1) $value)
      ;;
      ((perform (op vector-set!) (reg the-cdrs) (reg $reg1) $value)
       ))
     (;; rewrite `cons`
      (assign $reg1 (op cons) $val1 $val2)
      ;;
      ((perform (op vector-set!) (reg the-cars) (reg free) $val1)
       (perform (op vector-set!) (reg the-cdrs) (reg free) $val2)
       (assign $reg1 (reg free))
       (assign free (op ptr-inc) (reg free))
       ))
     ))

(define stack-manip-rules
  `( (;; rewrite "save"
      (save $reg)
      ;;
      ((assign the-stack (op cons) (reg $reg) (reg the-stack))
       ))
     (;; rewrite "restore"
      (restore $reg)
      ;;
      ((assign $reg (op car) (reg the-stack))
       (assign the-stack (op cdr) (reg the-stack))))
     (;; rewrite stack initialization
      (perform (op initialize-stack))
      ;;
      ((assign the-stack (const ()))
       ))
     ))

;; TODO: we need a function here to expand the list of instructions
;; as long as there is at least one applicable rule
;; and also we need to make sure that we don't write rules that expands
;; infinitely.
;; we need this function because the stack manipulations are still using
;; list primitives like "car" "cdr" and "cons", and the reason we have this
;; rewrite system is that we are trying to avoid doing these tasks manually.
;; it will be great if we can just write down the most intuitive code
;; and leave all the boring tasks to computer.

(define (rewrite-instructions rules insns)
  (let loop ((new-insns '())
             (curr-insns insns))
    (if (null? curr-insns)
        new-insns
        (let ((result (try-rewrite-once rules (car curr-insns))))
          (loop (append new-insns
                        (or result (list (car curr-insns))))
                (cdr curr-insns))))))
