;; TODO: as always, if we can make this work with simu.scm
;; the same should be true for the legacy one

(load "../common/utils.scm")
(load "../common/test-utils.scm")
(load "./simu.scm")

(load "./ec-eval.scm")
(load "./ec-prim.scm")

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

;; TODO: guess we can now abstract out the machine part
;; making the whole process a scheme procedure
;; and write some unit tests

;; note that we have 2 kinds of primitives:
;; one kind of the primitives are for the machine,
;; which implement the functionality of an evaluator
;; and another kind of the primitives for the implemented language,
;; which provides the functionality so that we can do something useful
;; in the implemented language.
;; "to-machine-prim-entry" lifts primitives to the machine
;; (TODO) "to-eval-prim-entry" lifts primitives so that it's visible
;; from the implemented language.

(define (to-eval-prim-entry sym)
  `(,sym ,(lift-primitive (eval sym user-initial-environment))))

;; initialize an environment that contains basic stuff
(define (init-env)
  (let ((proc-list
         (map to-eval-prim-entry
              '(
                + - * /
                = > >= < <=
                zero? eq? eqv?
                car cdr cons null?
                list even? odd?
                not remainder quotient
                sqrt integer?
                member memq delete
                abs append
                ))))
    (extend-environment
     '(true false)
     '(#t   #f)
     (extend-environment
      (map car proc-list)
      (map cadr proc-list)
      the-empty-environment))))

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

(out
 (machine-eval '(+ 1 2 3) (init-env)))

(end-script)
