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
     ;; TODO:
     ;; instead of capturing (reg $regX)
     ;; we could try to capture $srcX,
     ;; by doing this we also make it possible
     ;; to translate things like
     ;; (cons <constant1> <constant2>)
     ;; but we will lose the power of telling if an integer
     ;; stands for a pair (i.e. memory address) or an number
     ;; for now I don't know if it is beneficial
     ;; to do this change,
     ;; but as we have more exercises available,
     ;; the intention of doing so will be more clear.
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

;; TODO:
;; we need some serious rework to get this to work
;; first of all we need every value tied with a label
;; indicating its type, making `null?` `pair?` `symbol?` `number?` possible.
;; so here we should limit what kind of data can be used with `(const <data>)`
