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

;; remove duplicates and 'pc & 'flag registers, sort
;; making it ready to use as a regular register name list
(define (merge-register-lists
         reg-names-1 reg-names-2)
  (sort
   (remove-duplicates
    (set-diff
     (apply append (list reg-names-1 reg-names-2))
     '(pc flag)))
   (lambda (x y)
     (string<=? (symbol->string x)
                (symbol->string y)))))

(define (make-and-execute-with
         ;; controller-text is assumed always
         ;; being the following form:
         ;; '(controller <then> <come> <instructions> ...)
         ;; so we can safely drop the first element
         controller-text
         reg-bindings
         primitive-list)
  (let* ((insns (cdr controller-text))
         (reg-names-1 (extract-register-names insns))
         (reg-names-2 (map car reg-bindings))
         (reg-names (merge-register-lists
                     reg-names-1
                     reg-names-2))
         (m (make-machine
             reg-names
             primitive-list
             insns)))
    (for-each
     (lambda (pair)
       (set-register-contents! m (car pair) (cadr pair)))
     reg-bindings)
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
