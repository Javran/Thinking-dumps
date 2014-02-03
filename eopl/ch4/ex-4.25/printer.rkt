(module printer (lib "eopl.ss" "eopl")

  (require (only-in racket format))

  (provide
    print-to-screen
    initialize-printer!
    get-printed
    send-to-printer)
  
  (define print-to-screen (make-parameter #f))

  ; a reversed list of all string printed
  (define printed 'uninitialized)

  ; like a white paper
  (define (empty-printed) '())

  (define (initialize-printer!)
    (set! printed (empty-printed)))

  ; get all the printed strings,
  ;   in time order
  (define (get-printed)
    (reverse printed))

  (define (send-to-printer obj)
    (define next-line
      (format "~A" obj))
    (set! printed
      (cons next-line
            printed))
    (when (print-to-screen)
      (display next-line)
      (newline)))

  )
