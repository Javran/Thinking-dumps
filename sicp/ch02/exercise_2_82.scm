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
    (display "type change: ")
    (display (type-tag x))
    (display " -> ")
    (display new-tag)
    (newline)
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

(put 'test '(type-c type-c type-c)
     (lambda (a b c)
       (out "Test ok." a b c)))

(load "./exercise_2_82_apply.scm")

(define (test a b c)
  (apply-generic 'test a b c))

(newline)
(test (make-type-c 1) (make-type-c 2) (make-type-c 3))
(test (make-type-a 4) (make-type-a 5) (make-type-c 6))
(test (make-type-a 7) (make-type-b 8) (make-type-c 9))

; situation where this strategy is not sufficiently general:
; consider we have a multiple procedure that multiple a complex number by a real number
; the type would be '(complex real) or '(real complex)
; when the apply-generic is call with type '(integer complex), there would not be an exact match
; and system will attempt to make all types of the argument the same, in this case if a suitable handler
; (e.g. multiple procedure that matches '(complex complex) ) cannot be found, the apply-generic will fail

(end-script)
