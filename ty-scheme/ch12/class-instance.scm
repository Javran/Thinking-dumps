(load "../common/utils.scm")
(load "../common/defstruct.scm")

(define debug-flag #t)

; make an instance
(define make-instance
  (lambda (class . slot-value-pairs)
    
    ; actually an instance is a vector,
    ; where the first element is class
    ; and others are pairs of slot & value

    (let* (
        (slot-list (standard-class.slots class))
        (slot-count (length slot-list))
        ; we need one extra place to keep class
        (instance (make-vector (+ slot-count 1)))

        )
      ; first element is the class itself
      (vector-set! instance 0 class)

      (let loop ((cur-slot-value-pairs slot-value-pairs))
        (if (null? cur-slot-value-pairs)
          instance ; nothing remaining, return the instance
          (let* (
              ; (cur-slot:cur-val:rest-slot-value-pairs) = (cur-slot-value-pairs)
              (cur-slot (car cur-slot-value-pairs))
              (cur-val (cadr cur-slot-value-pairs))
              (rest-slot-value-pairs (cddr cur-slot-value-pairs))
              ; find cur-slot index in slot-list
              (slot-pos (list-position cur-slot slot-list))
              )
            (vector-set! instance (+ slot-pos 1) cur-val)

            (if debug-flag
              (begin
                (display "position: ")
                (display slot-pos)
                (display " with value: ")
                (display cur-val)
                (newline)))
            (loop rest-slot-value-pairs)))))))

(define standard-class
  (vector
    ; class
    'place-holder
    ; slots
    (list
      'slots
      'superclass
      'method-names
      'method-vector)
    ; superclass
    #t
    ; method-names
    '(make-instance)
    (vector make-instance)))
(vector-set! standard-class 0 standard-class)

(define standard-class?
  (lambda (x)
    (and (vector? x) (eqv? (vector-ref x 0) standard-class))))

(define standard-class.slots
  (lambda (c)
    (vector-ref c 1)))

(define standard-class.superclass
  (lambda (c)
    (vector-ref c 2)))

(define standard-class.method-names
  (lambda (c)
    (vector-ref c 3)))

(define standard-class.method-vector
  (lambda (c)
    (vector-ref c 4)))

(define make-standard-class
  (lambda args
    (apply send 'make-instance standard-class args))) 

(define send
  (lambda (method instance . args)
    (let (
        ; search method through superclass-chain
        (proc (let loop ((class (class-of instance)))
                (if (eqv? class #t)
                  ; root reached
                  (error 'root-reached)
                  ; else
                  (let (
                      (method-pos (list-position
                                    method
                                    (standard-class.method-names class))))
                    (if method-pos
                      ; the method is found
                      (vector-ref (standard-class.method-vector class) method-pos)
                      ; else
                      (loop (standard-class.superclass class))))))))
      (apply proc instance args))))

; extract class from instance
(define class-of
  (lambda (x)
    (if (vector? x)
      (let ((n (vector-length x)))
        (if (>= n 1)
          (let ((c (vector-ref x 0)))
            (if (standard-class? c) c #t))
          #t))
      #t)))

; make a class
(define trivial-bike-class
  (make-standard-class
    'superclass #t
    'slots '(frame parts size)
    'method-names '()
    'method-vector #()))

(define my-bike
  (make-instance trivial-bike-class
    'frame 'cromoly
    'size '18.5
    'parts 'alivio))
