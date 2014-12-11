(load "./ec-eval.scm")

;; evaluate a symbol under the current toplevel
;; environment, and make it an primitive entry
(define (to-machine-prim-entry sym)
  `(,sym ,(eval sym user-initial-environment)))

(define default-ops-builder
  (let ((old-builder default-ops-builder))
    (lambda (m)
      (let* ((old-ops (old-builder m))
             (new-prim-symbols
              ;; only add those that don't show up
              ;; in the old primitive list ...
              (set-diff ec-required-operations
                        (map car old-ops))))
        `(
          ;; we are trying to be lazy here by:
          ;; * extract the list of required operation names direcly
          ;;   from the code of the evaluator
          ;; * operation names are symbols, and as we have implemented
          ;;   them somewhere in our toplevel user environment
          ;;   we can evaluate them directly to convert each operation symbol
          ;;   to its corresponding primitive entry
          ,@(map to-machine-prim-entry new-prim-symbols)
          ,@(old-builder m))))))

;; use the machine to evlauate a lisp expression
(define (machine-eval exp env)
  (let* ((entry-label (gensym))
         (exit-label (gensym))
         (eval-label (gensym))
         (m (build-and-execute
             `(controller
               (goto (label ,entry-label))
               ,eval-label
               ,@evaluator-insns
               ,entry-label
               (assign continue (label ,exit-label))
               (goto (label ,eval-label))
               ,exit-label)
             `((exp ,exp)
               (env ,env)))))
    (machine-reg-get m 'val)))

(define (check-labels insns)
  (let ((labels (filter symbol? insns)))
    (assert (= (length labels)
               (length (remove-duplicates labels)))
            "labels are supposed to be unique")
    (define (extract-used-labels insn)
      (cond ((and (pair? insn)
                  (or (eq? (car insn) 'branch)
                      (eq? (car insn) 'goto))
                  (eq? (car (cadr insn)) 'label))
             (list (cadr (cadr insn))))
            (else '())))
    (let ((used-labels
           (remove-duplicates
            (concat-map extract-used-labels insns))))
      (for-each
       (lambda (l)
         (if (memq l labels)
             'ok
             (warn (format #f "label ~A not defined" l))))
       used-labels)
      'ok)))
