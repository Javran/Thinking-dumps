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

(define delete-duplicates
  (lambda (s)
    (if (null? s)
      s
      (let (
          (head (car s))
          (tail (cdr s))
          )
        ; memv - if head is equal to one of the member of tail
        ; please refer to:
        ; http://stackoverflow.com/questions/1869116/scheme-built-in-to-check-list-containment/
        (if (memv head tail)
          ; then head is not needed
          (delete-duplicates tail)
          ; else add head in the list
          (cons head (delete-duplicates tail)))))))

; makes appropriate call to make-standard-class for us
(define create-class-proc
  (lambda (superclass slots method-names method-vector)
    (make-standard-class
      'superclass superclass
      'slots
        (let (
            (superclass-slots
              ; if the superclass is not the root class (i.e. #t)
              (if (not (eqv? superclass #t))
                ; derive all slots from superclass
                (standard-class.slots superclass)
                ; else, just return an empty list
                '())))
          (if (null? superclass-slots)
            slots
            ; append slots from superclass, and remove duplicate slots
            (delete-duplicates
              (append slots superclass-slots))))
      'method-names method-names
      'method-vector method-vector)))

(define-syntax create-class
  (rsc-macro-transformer
    (let ((xfmr
      (lambda (superclass slots . methods)
      `(create-class-proc
         ; superclass
         ,superclass
         ; slots
         (list ,@(map (lambda (slot) `',slot) slots))
         ; method-names
         (list ,@(map (lambda (method) `',(car method)) methods))
         ; method-vector
         (vector ,@(map (lambda (method) `,(cadr method)) methods))))))
      (lambda (e r)
        (apply xfmr (cdr e))))))

