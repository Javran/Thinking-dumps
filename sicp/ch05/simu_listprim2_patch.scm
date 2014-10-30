;; the real implementation
;; of list primitives

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
     ;; TODO: what if I want to do something like:
     ;; (set-car! x 1) ???
     (;; rewrite `set-car!`
      (perform (op set-car!) (reg $reg1) (reg $reg2))
      ;;
      ((perform (op vector-set!) (reg the-cars) (reg $reg1) (reg $reg2))
       ))
     (;; rewrite `set-cdr!`
      (perform (op set-cdr!) (reg $reg1) (reg $reg2))
      ;;
      ((perform (op vector-set!) (reg the-cdrs) (reg $reg1) (reg $reg2))
       ))
     (;; rewrite `cons`
      (assign $reg1 (op cons) (reg $reg2) (reg $reg3))
      ;;
      ((perform (op vector-set!) (reg the-cars) (reg free) (reg $reg2))
       (perform (op vector-set!) (reg the-cdrs) (reg free) (reg $reg3))
       (assign $reg1 (reg free))
       (assign free (op +) (reg free) (const 1))
       ))
     ))
