(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "exercise_5_40_ctenv.scm")

;; TODO: lexical address's structure
;; is previously (<frame-layer> . <position>)
;; and we need to fix this.

(define (find-variable var ctenv)
  ;; poor design, why not just use "#f"
  ;; or let it crash?
  (if (empty-ctenv? ctenv)
      'not-found
      (if (member var (first-ctframe ctenv))
          (let loop ((i 0)
                     (xs (first-ctframe ctenv)))
            ;; the predicate "member var (first-ctframe ctenv)"
            ;; guarantees that "xs" cannot be empty
            ;; and it must contain one matching element.
            (if (equal? (car xs) var)
                (list 0 i)
                (loop (add1 i) (cdr xs))))
          (let ((result (find-variable var (enclosing-ctenv ctenv))))
            ;; since the failure flag is special
            ;; we have to pass the failure flag around
            (if (equal? 'not-found result)
                'not-found
                (list (add1 (car result))
                      (cadr result)))))))

(define test-ctenv
  '( (y z)
     (a b c d e)
     (x y)
     ))

(out (find-variable 'c test-ctenv)
     (find-variable 'x test-ctenv)
     (find-variable 'w test-ctenv))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
