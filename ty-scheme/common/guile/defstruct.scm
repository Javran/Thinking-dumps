; get the position of obj in the list
(define list-position
  ; the function accepts 2 arg: obj and ls
  (lambda (obj ls)
    ; initialize index to 0, list(l) to ls
    (let loop ((i 0) (l ls))
      (if (null? l) 
        #f ; the list is empty, nothing can be found
        (if (eqv? (car l) obj) i ; ok, we've found the obj in list
          (loop (+ i 1) (cdr l)))))))

(define undefined (if #f #f))

; defstruct accepts structure name & field definitions
; * a field definition is either a symbol or a (symbol default_value) pair
(define-macro defstruct
      (lambda (struct-name . field-defs)
        (let* (
          ; the argument is either a symbol or a (symbol default_value) pair
          ;   returns the symbol
          (symbol-of (lambda (x) (if (pair? x) (car x) x)))
          ; keep structure name as string struct-name-str
          (struct-name-str (symbol->string struct-name)) 
          ; keep field count
          (field-count (length field-defs))
          (field-syms (map symbol-of field-defs))
          (struct-size (+ field-count 1))
          (default-field-values 
            (list->vector
              (cons 
                struct-name
                (map 
                  (lambda (field) (if (pair? field) (cadr field) undefined))
                  field-defs))))

          ; function names
          (maker-func (string->symbol (string-append "make-" struct-name-str)))
          (recog-func (string->symbol (string-append struct-name-str "?")))
          (getter-func (lambda (field-name)
                         (string->symbol (string-append struct-name-str "." field-name))))
          (setter-func (lambda (field-name)
                         (string->symbol (string-append "set!" struct-name-str "." field-name))))
          )

          `(begin
            ; definition of the constructor
            (define ,maker-func
              ; the constructor should be followed with field-value pairs
              (lambda field-value-pairs
                (let (
                      ; make room for fields
                      (inst (make-vector ,struct-size))
                      ; eval & put field symbol list here
                      (field-sym-list ',field-syms))

                  ; the first element is the structure name
                  (vector-set! inst 0 ',struct-name)

                  ; set each field with its corresponding default value
                  ,@(let loop ((i 1) (r '()))
                      (if (> i field-count)
                        r
                        (loop (+ i 1)
                              (cons `(vector-set! inst ,i ,(vector-ref default-field-values i)) r))))

                  ; look through field names, override default values with assigned one
                  (let loop ((cur-field-value-pairs field-value-pairs))
                    (if (not (null? cur-field-value-pairs))
                      (begin
                        (vector-set!
                          inst 
                          (+ (list-position (car cur-field-value-pairs) field-sym-list) 1)
                          (cadr cur-field-value-pairs))
                        (loop (cddr cur-field-value-pairs)))))
                  inst)))

             ; defintion of field getter & setter
             ; start from the first field i.e. i = 1
             ,@(let loop ((i 1) (procs '()))
                 (if (> i field-count) procs
                     (loop (+ i 1)
                           ; get name of a field -> f
                           (let ((f (symbol->string (list-ref field-syms (- i 1)))))
                             (cons
                               ; the getter
                               `(define ,(getter-func f) (lambda (x) (vector-ref x ,i)))
                                (cons
                                  ; the setter
                                  `(define ,(setter-func f) (lambda (x v) (vector-set! x ,i v)))
                               procs))))))

             ; definition of the recognizing function
             (define ,recog-func (lambda (x)
                 (and 
                   ; the instance should be a vector
                   (vector? x)
                   ; the first element should be the name of a structure
                   (eqv? (vector-ref x 0) ',struct-name))))

                        ))))
