;; extract register names (except for "pc" and "flag")
;; from the controller text
;; only target registers in "assign" instructions and "restore" instructions
;; are taken into account
;; but this is sufficient, since no other instructions can modify a register
;; "pc" and "flag" are always removed from the result
(define (extract-register-names controller-text)
    (define (extract insn)
      (cond
       ((or (tagged-list? insn 'assign)
            (tagged-list? insn 'restore))
        (list (cadr insn)))
       (else '())))
    (sort
     (remove-duplicates
      (set-diff
       (apply append (map extract controller-text))
       '(pc flag)))
     (lambda (x y)
       (string<=? (symbol->string x)
                  (symbol->string y)))))

;; build a machine with controller-text assembled,
;; registers assigned according to the table,
;; and primitive operations specified
(define (build-machine-with controller-text init-reg-tables ops)
  (let ((m (empty-machine))
        (reg-names (extract-register-names controller-text)))
    (machine-define-registers!
     m reg-names)
    (machine-set-operations! m ops)
    (assemble controller-text m)
    (machine-reset-pc! m)
    (machine-execute! m)
    m))
