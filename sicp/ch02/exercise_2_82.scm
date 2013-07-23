(load "../common/utils.scm")
(load "./5_2_coercion_base.scm")

; let's make a simple type system:
; type-a can be converted to type-b or type-c
; type-b can be converted to type-c
;
; to simplify the implementation, we define type-a->type-c explicitly
; other than infer this conversion from table

(define (change-to-tag new-tag)
  (lambda (x)
    (attach-tag new-tag (contents x))))

(put-coercion 'type-a 'type-b (change-to-tag 'type-b))
(put-coercion 'type-b 'type-c (change-to-tag 'type-c))
(put-coercion 'type-a 'type-c (change-to-tag 'type-c))

(define (type-builder type)
  (lambda (x)
    (attach-tag type x)))

(define make-type-a (type-builder 'type-a))
(define make-type-b (type-builder 'type-b))
(define make-type-c (type-builder 'type-c))

(out (make-type-a 'stub-a))
(out (make-type-b 'stub-b))
(out ((get-coercion 'type-b 'type-c)
       (make-type-b 'stub-c)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        ; else
        (if (= (length args) 2)
          (let ((type1 (car type-tags))
                (type2 (cadr type-tags))
                (a1 (car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2 
                      (apply-generic op (t1->t2 a1) a2))
                    (t2->t1
                      (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types"
                                 (list op type-tags))))))
          (error "No method for these type"
                 (list op type-tags)))))))

(end-script)
