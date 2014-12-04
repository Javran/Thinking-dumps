;; TODO: as always, if we can make this work with simu.scm
;; the same should be true for the legacy one

(load "../common/utils.scm")
(load "../common/test-utils.scm")
(load "./simu.scm")

(load "./ec-eval.scm")
(load "./ec-prim.scm")

;; evaluate a symbol under the current toplevel
;; environment, and make it an primitive entry
(define (to-prim-entry sym)
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
          ,@(map to-prim-entry new-prim-symbols)
          ,@(old-builder m))))))

;; TODO: guess we can now abstract out the machine part
;; making the whole process a scheme procedure
;; and write some unit tests

(out
 (default-ops-builder (empty-machine)))

(end-script)
