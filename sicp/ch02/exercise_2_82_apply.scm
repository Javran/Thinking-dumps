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
                 (lambda (type-to)
                   (let ((converters
                           (map (lambda (type-from)
                                  (if (equal? type-from type-to)
                                    identity
                                    (get-coercion type-from type-to)))
                                type-tags)))
                     (if (and
                           ; all converters should be available
                           (apply boolean/and converters)
                           (get op (map (const type-to) type-tags)))
                       converters
                       #f))))
               (solutions
                 (filter identity 
                         (map get-converters type-tags))))
          (if (null? solutions)
            (error "No method for these types"
                   (list op type-tags))
            ; take the first solution and then zip!
            (let ((new-args (map (lambda (f data)
                                   (f data))
                                 (car solutions)
                                 args)))
              (apply apply-generic (cons op new-args)))))))))
