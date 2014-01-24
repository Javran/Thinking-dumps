(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.rkt")                  ; for expression?
  (require "store.rkt")                 ; for reference?
  (require (only-in racket foldl))

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference. 

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?))
    (ref-val
      (ref reference?))
    )

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

(define expval->ref
    (lambda (v)
      (cases expval v
	(ref-val (ref) ref)
	(else (expval-extractor-error 'reference v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define (pred-or pred1 pred2)
    (lambda (x)
      (or (pred1 x)
          (pred2 x))))

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))

  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval (pred-or reference? vector?))                 ; new for implicit-refs
      (saved-env environment?)))

  (define (extend-env-rec* p-names b-vars bodies saved-env)
    (let ((vecs (map (lambda (x) (make-vector 1)) p-names)))
      (define new-env
        (foldl
          extend-env
          saved-env
          p-names
          vecs))
      (for-each
        (lambda (p-name b-var body vec)
          (vector-set! vec 0
            (proc-val (procedure b-var body new-env))))
        p-names
        b-vars
        bodies
        vecs)
      new-env))

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
	      (empty-env () '())
        (extend-env (sym val saved-env)
          (if (vector? val)
            (cons
              (list sym "<proc>")
              (env->list saved-env))
            (cons
              (list sym val)              ; val is a denoted value-- a
              ; reference. 
              (env->list saved-env)))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list 
  (define expval->printable
    (lambda (val)
      (cases expval val
        (proc-val (p)
          (cases proc p
            (procedure (var body saved-env)
              (list 'procedure var '... (env->list saved-env)))))
	(else val))))

)
