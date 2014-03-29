(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for letrec-lang.

  (require "lang.rkt")                  ; for expression?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val 
      (proc proc?)))

;;; extractors:

  ;; expval->num : ExpVal -> Int
  (define expval->num
    (lambda (v)
      (cases expval v
	(num-val (num) num)
	(else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  (define expval->bool
    (lambda (v)
      (cases expval v
	(bool-val (bool) bool)
	(else (expval-extractor-error 'bool v)))))

  ;; expval->proc : ExpVal -> Proc
  (define expval->proc
    (lambda (v)
      (cases expval v
	(proc-val (proc) proc)
	(else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; proc? : SchemeVal -> Bool
  ;; procedure : Var * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))

  (define (env-val? exp)
    (or (expval? exp)
        (vector? exp)))

  ;; Page: 86
  ; define that if `bval` is vector,
  ;   we ignore `bvar` and look into the structure of `bval`
  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval env-val?)
      (saved-env environment?)))

  ; extend-env-rec: [Symbol] x [Symbol] x [Exp] x Env -> Env
  (define (extend-env-rec p-names b-vars bodys saved-env)
    (let ((vec (make-vector 1)))
      ; would never use `bvar` in this case,
      ;   so it's save to assign it an arbitary value
      (let ((new-env (extend-env 'stub vec saved-env)))
        (define proc-alist
          (map
            (lambda (p-name b-var body)
              (list
                p-name
                (proc-val (procedure b-var body new-env))))
            p-names
            b-vars
            bodys))
        (vector-set! vec 0 proc-alist)
        new-env)))

)
