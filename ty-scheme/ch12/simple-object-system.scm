(load "../common/utils.scm")
(load "../common/defstruct.scm")
(load "../common/alist.scm")

(define debug-flag #t)

(defstruct standard-class
  slots superclass method-names method-vector)

; make a class
(define trivial-bike-class
  (make-standard-class
    'superclass #t
    'slots '(frame parts size)
    'method-names '()
    'method-vector #()))

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


(define my-bike
  (make-instance trivial-bike-class
    'frame 'cromoly
    'size '18.5
    'parts 'alivio))

; check the detail of an instance
(out my-bike)

; the correct order of vector should be:
; #(<trivial-bike-class> cromoly alivio 18.5)
; following the slot definition: (frame parts size)

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

(out (eqv? (class-of my-bike) trivial-bike-class))
; #t

(out
  (map class-of (list 1 1.5 #\a "nice" my-bike)))
; anything that does not made from 'standard-class' returns #t
; (#t #t #t #t <trivial-bike-class>)

; getter and setter
(define slot-value
  (lambda (instance slot)
    (let* (
        (class (class-of instance))
        (slot-index (list-position slot (standard-class.slots class)))
        )
      (vector-ref instance (+ slot-index 1)))))

(define set!slot-value
  (lambda (instance slot new-val)
    (let* (
        (class (class-of instance))
        (slot-index (list-position slot (standard-class.slots class)))
        )
      (vector-set! instance (+ slot-index 1) new-val))))
