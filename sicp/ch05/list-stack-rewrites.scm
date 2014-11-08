;; rewrite rules for list primitives and stacks

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
