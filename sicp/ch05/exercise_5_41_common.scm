(load "exercise_5_40_ctenv.scm")

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
