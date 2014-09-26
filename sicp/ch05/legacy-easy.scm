;; this module should be imported instead of "legacy-simu.scm"
;; as I'll try to make less lines of code necessary to run
;; and test a machine model

(load "./legacy-simu.scm")

(define (remove-duplicates xs)
  (if (null? xs)
      '()
      (cons (car xs)
            (delete
             (car xs)
             (remove-duplicates (cdr xs))))))

(define (set-diff xs ys)
  (fold-right delete xs ys))

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
     ))

;; from "simu" modules
(define (extract-register-names instructions)
  (define (extract insn)
    (cond
     ((or (tagged-list? insn 'assign)
          (tagged-list? insn 'restore))
      (list (cadr insn)))
     (else '())))
  (remove-duplicates
   (set-diff
    (apply append (map extract instructions))
    '(pc flag))))

;; use `initialize-registers!` to [re-] initialize register values
;; and then `start` to [re-] start machine execution
(define (ctl-ops->machine
         controller-text
         primitive-list)
  (let* ((insns (cdr controller-text))
         (reg-names (extract-register-names insns))
         (m (make-machine
             reg-names
             primitive-list
             insns)))
    m))

(define (ctl->machine
         controller-text)
  (ctl-ops->machine
   controller-text
   default-primitive-list))

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
