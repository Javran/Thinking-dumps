;; this module should be imported instead of "legacy-simu.scm"
;; as I'll try to make less lines of code necessary to run
;; and test a machine model

(load "legacy-simu.scm")

(define (set-diff xs ys)
  (fold-right delete xs ys))

(define (make-machine register-names
                      ops
                      controller-text)
  (let ((machine (make-new-machine)))
    (for-each
     (lambda (register-name)
       ((machine 'allocate-register) register-name))
     ;; remvoe "pc" and "flag" when
     ;; we are creeating the machine,
     ;; because these two registers have been created beforehand
     (remove-duplicates
      (set-diff register-names
                '(pc flag))))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; make it a procedure, so one change to an instance
;; of it won't cause too many problems
(define (default-primitive-list)
  `( (+ ,+)
     (- ,-)
     (* ,*)
     (/ ,/)
     (zero? ,zero?)
     (> ,>)
     (>= ,>=)
     (< ,<)
     (<= ,<=)
     (= ,=)
     (square ,square)
     (average ,average)
     (abs ,abs)
     (odd? ,odd?)
     (even? ,even?)
     (print ,out)
     (not ,not)
     ))

;; copied from simu.scm
(define (extract-register-names instructions)
  (define (extract insn)
    (if (symbol? insn)
        '()
        (let ((names1
               ;; special targets that does not have an "reg"
               ;; explicitly
               (cond
                ((or (tagged-list? insn 'assign)
                     (tagged-list? insn 'save)
                     (tagged-list? insn 'restore))
                 (list (cadr insn)))
                (else '())))
              (names2
               ;; registers indicated by "reg"
               (map cadr
                    (filter (lambda (e)
                              (and (list? e)
                                   (eq? 'reg (car e))))
                            insn))))
          (append names1 names2))))
  (remove-duplicates
    (concat-map extract instructions)))

;; patches that want to transform the instruction
;; list before assembling can rewrite this procedure
(define tranform-instructions
  identity)

;; use `initialize-registers!` to [re-] initialize register values
;; and then `start` to [re-] start machine execution
(define (ctl-ops->machine
         controller-text
         primitive-list)
  (let* ((origin-insns (cdr controller-text))
         (insns (tranform-instructions origin-insns))
         (reg-names (extract-register-names insns))
         (m (make-machine
             reg-names
             primitive-list
             insns)))
    m))

(define (initialize-registers! m reg-bindings)
  (for-each
   (lambda (pair)
     (set-register-contents! m (car pair) (cadr pair)))
   reg-bindings))

;; make but without execution
(define (make-with
         ;; controller-text is assumed always
         ;; being the following form:
         ;; '(controller <then> <come> <instructions> ...)
         ;; so we can safely drop the first element
         controller-text
         reg-bindings
         primitive-list)
  (let* ((m (ctl-ops->machine
             controller-text
             primitive-list)))
    (initialize-registers! m reg-bindings)
    m))

(define (make-and-execute-with
         controller-text
         reg-bindings
         primitive-list)
  (let ((m (make-with
            controller-text
            reg-bindings
            primitive-list)))
    (start m)
    m))

;; make and execute the machine
;; using the default list of primitives
(define (make-and-execute
         controller-text
         reg-bindings)
  (make-and-execute-with
   controller-text
   reg-bindings
   (default-primitive-list)))

;; find the first duplicated element
;; if no duplicate element is found, return #t
;; otherwise a list will be returned
;; whose first element is the duplicate one
;; e.g. (a b c d) => #f
;;      (a b a d) => (a b a d)
;;      (a b c d e c) => (c d e c)
(define (first-dup-element xs)
  (if (null? xs)
      #f
      (if (member (car xs) (cdr xs))
          xs
          (first-dup-element (cdr xs)))))

;; scan through the instruction list,
;; raise an error immediately when multiple labels
;; with the same name is detected
(define (scan-duplicate-labels insns label?)
  (let* ((labels (filter label? insns))
         (dup-lbl (first-dup-element labels)))
    (if dup-lbl
        ;; here we can even report all the labels with
        ;; the same name, but I don't find it not very useful.
        (error "multiple labels with the same name:"
               (car dup-lbl))
        'ok)))

;; overwrite original implementation
;; since the definition of "extract-labels" is in top level
(define (extract-labels text receive)
  (scan-duplicate-labels text symbol?)
  (if (null? text)
      (receive '() '())
      (extract-labels
       (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (if (symbol? next-inst)
               (receive insts
                   (cons (make-label-entry
                          next-inst
                          insts)
                         labels))
               (receive (cons (make-instruction next-inst)
                              insts)
                   labels)))))))

;; check a list of instructions including labels
;; * force every label to be unique
;; * warn if the is any undefined by used labels
;; #f will be returned if there is any warning
(define (check-labels insns)
  (let ((labels (filter symbol? insns)))
    ;; if all the labels are unique
    ;; then turning it into a set shouldn't
    ;; make a different on the size of it
    (assert (= (length labels)
               (length (remove-duplicates labels)))
            "labels are supposed to be unique")
    ;; extract a list of labels from a single instruction
    (define (extract-used-labels insn)
      (cond ((and
              ;; it's possible to use "car"
              (pair? insn)
              ;; the instruction is (branch ...) or (goto ...)
              (or (eq? (car insn) 'branch)
                  (eq? (car insn) 'goto))
              ;; more precisely, should be of the following form:
              ;; (<branch|goto> (label <label>) ...)
              ;; "cadr" on the instruction finds "(label <label>)"
              ;; and another "cadr" on this result will extract
              ;; that very label
              (eq? (car (cadr insn)) 'label))
             (list (cadr (cadr insn))))
            (else '())))
    (let ((used-labels
           (remove-duplicates
            (concat-map extract-used-labels insns))))
      ;; are all used labels defined in this list itself?
      (let ((results
             (map
              (lambda (l)
                (if (memq l labels)
                    #t
                    (begin
                      (warn (format #f "label ~A not defined" l))
                      #f)))
              used-labels)))
        (fold-left (lambda (a b)
                     (and a b))
                   #t results)))))
