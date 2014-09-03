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
