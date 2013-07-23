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

(put 'test '(type-a type-a type-a)
     (lambda (a b c)
       (out "Test ok." a b c)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        ; find a exact match, apply it
        (apply proc (map contents args))
        ; else
        (let* ((type-tos type-tags) ; a list of target types we should attempt
               (get-converters
                 ; a procedure when given a type, it attempts to fetch a list of
                 ;  corresponding converters to each argument
                 (let ((converters
                         (lambda (type-to)
                           (map (lambda (type-from)
                                  (if (equal? type-from type-to)
                                    identity
                                    (get-coercion type-from type-to)))
                                type-tags))))
                   (if (and
                         ; all converters should be available
                         (apply and converters)
                         ; and the proc exists
                         (proc (get op (map (const type-to) type-tags))))
                     converters
                     #f)))
               (solutions
                 (filter identity 
                         (map get-converters type-tags))))
          (if (null? solutions)
            (error "No method for these types"
                   (list op type-tags))
            ; take the first solution and then zip!
            (apply proc (map apply
                             (car solution)
                             (map contents args)))))))))

(define (test a b c)
  (apply-generic 'test a b c))

(out (test (make-type-a 'a) (make-type-a 'b) (make-type-a 'c)))


(end-script)
