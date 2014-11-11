;; see notes in "5_3_2_maintaining_the_illusion_of_infinite_memory.md"
;; for related discussions

(load "./rewrite-instructions.scm")
(load "./list-stack-rewrites.scm")

(define (constant-exp-value exp)
  (define (valid-constant? data)
    ;; now data can only be one of:
    ;; symbol, number, boolean, string, char or null
    (or (symbol? data)
        (number? data)
        (boolean? data)
        (string? data)
        (char? data)
        (null? data)))
  (let ((data (cadr exp)))
    (if (valid-constant? data)
        data
        (error "cannot use" data
               "as a constant"))))

(define reserved-registers
  ;; "pc" and "flag" registers are not that special
  ;; it isn't a very good idea to make them special in the original design
  ;; because that design makes it complicated
  ;; when you want to reserve more registers
  '(pc flag the-cars the-cdrs the-stack))

(define (extract-register-names instructions)
  (define (extract insn)
    (if (symbol? insn)
        '()
        (let ((names1
               (cond
                ((or (tagged-list? insn 'assign)
                     (tagged-list? insn 'save)
                     (tagged-list? insn 'restore))
                 (list (cadr insn)))
                (else '())))
              (names2
               (map cadr
                    (filter (lambda (e)
                              (and (list? e)
                                   (eq? 'reg (car e))))
                            insn))))
          (append names1 names2))))
  (remove-duplicates
   (set-diff
    ;; make sure reserved registers exist
    (append
     (concat-map extract instructions)
     reserved-registers)
    ;; and delete "pc" and "flag"
    ;; because they are way more special
    ;; thanks to the original design
    '(pc flag))))
