;; the real implementation
;; of list primitives
;; I guess this will eventually replace the ad hoc
;; implementation of "listprim"

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

(define (rewrite-instructions rules insns)
  (let loop ((new-insns '())
             (curr-insns insns))
    (if (null? curr-insns)
        new-insns
        (let ((result (try-rewrite-once rules (car curr-insns))))
          (loop (append new-insns
                        (or result (list (car curr-insns))))
                (cdr curr-insns))))))
