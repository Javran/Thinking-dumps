;; TODO: as always, if we can make this work with simu.scm
;; the same should be true for the legacy one

;; TODO: cleanup

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

;; note that we have 2 kinds of primitives:
;; one kind of the primitives are for the machine,
;; which implement the functionality of an evaluator
;; and another kind of the primitives for the implemented language,
;; which provides the functionality so that we can do something useful
;; in the implemented language.
;; "to-machine-prim-entry" lifts primitives to the machine
;; "to-eval-prim-entry" lifts primitives so that it's visible
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

;; evaluate the expression using both the machine evaluator
;; and the real scheme evaluator under the default environment settings
;; for optional arguments, see:
;; http://web.mit.edu/scheme_v9.0.1/doc/mit-scheme-ref/Lambda-Expressions.html
(define (test-eval exp #!optional verbose)
  ;; verbose = true by default
  (let ((machine-result
         (machine-eval exp (init-env)))
        (lisp-result
         (eval exp (make-top-level-environment))))
    (if (equal? machine-result lisp-result)
        (if (or (default-object? verbose) verbose)
            (display ".")
            'done)
        (error "eval results are inconsistent:"
               "expecting" lisp-result
               "while getting" machine-result))))

(define test-exps
  `(
    ;; test self-evaluating
    10
    1.3
    "wow"
    #\space
    #\a
    #t
    ;; test variable
    (begin
      (define x +)
      (x 1 2 3))
    (begin
      (define x 10)
      (define y 20)
      (- x y))
    ;; test quoted
    '(a b c)
    'a
    '"wow"
    '(a (b c) (d e))
    ;; test assignment
    (begin
      (define y 10)
      (set! y 20)
      (set! y (+ y y))
      y)
    ;; test definition
    (begin
      ;; tested already
      (define a 'a)
      a)
    ;; test if
    (begin
      (define x 1)
      (if #t
          (set! x 10)
          (set! x 20))
      x)
    (begin
      (define y 1)
      (if #f
          (set! y 10)
          (set! y 20))
      y)
    ;; test lambda
    (begin
      (define f
        (lambda (x)
          (lambda (y)
            (+ x x y))))
      ((f 10) 20))
    ;; test begin
    (begin
      1)
    (begin
      (+ 10 20)
      (+ 20 30))
    (begin
      (define x 10)
      (define y 20)
      (+ x x y))
    (begin
      (define x 10)
      (set! x 20)
      x)
    ;; test application
    (begin
      (define f
        (lambda (a b c d)
          (* (+ a b) (- c d))))
      (f 10 20 30 40))
    ))
(for-each test-eval test-exps)

(end-script)
