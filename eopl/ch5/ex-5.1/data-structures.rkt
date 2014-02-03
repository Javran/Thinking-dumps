(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.rkt")                  ; for expression?
  (require "../../common.rkt")

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

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

  ;; Page: 148
  (define identifier? symbol?)
#|
  (define-datatype continuation continuation?
    (end-cont)                 
    (zero1-cont
      (saved-cont continuation?))
    (let-exp-cont
      (var identifier?)
      (body expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (if-test-cont 
      (exp2 expression?)
      (exp3 expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (diff1-cont                
      (exp2 expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (diff2-cont                
      (val1 expval?)
      (saved-cont continuation?))
    (rator-cont            
      (rand expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (rand-cont             
      (val1 expval?)
      (saved-cont continuation?)))
|#


  ; procedural representaion:
  ; (list 'cont <type> <accessor0> <accessor1> ...)
  (define (continuation? k)
    (and (non-empty? k)
         (eqv? (car k) 'cont)))

  ; return the type of continuation `k`
  (define cont-type cadr)

  ; make continuation
  ; type: a symbol, indicating the type
  ; field-preds: the predicate required by this field
  ; the return value is a function (i.e. the constuctor)
  ;   when this function is called with suitable amount of
  ;   and suitable type of values, returns the continuation
  (define (make-continuation type . field-preds)
    ; (field-maker data): check `data` against `pred`
    ;   and yield a function with no argument which
    ;   when called returns `data`
    (define (field-pred->field-maker pred)
      (define (field-maker data)
        (assert
          (pred data)
          (format "invalid field value: ~A" data))
        (lambda () data))
      field-maker)
    (define (constructor . field-vals)
      (assert (= (length field-preds)
                 (length field-vals))
        "field count mismatch")
      `(cont ,type
              ,@(map (lambda (pred data)
                       ((field-pred->field-maker pred) data))
                   field-preds field-vals)))
    ; extractor fields from a continuation
    ;   and call proc with field values as arguments
    (define (extractor cont proc)
      (apply proc
        (map (lambda (f) (f))
             (cddr cont))))
    (cons constructor extractor))
          
  ; -p: pair
  ; -c: constructor
  ; -e: extractor
  (define end-cont-p
    (make-continuation 'end))

  (define zero1-cont-p
    (make-continuation 'zero1
      continuation?))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))
  
;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

  (define-datatype environment environment?
    (empty-env)
    (extend-env 
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec
      (p-name symbol?)
      (b-var symbol?)
      (p-body expression?)
      (saved-env environment?)))

)
