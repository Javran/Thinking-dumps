;; dealing with generating unique labels
;; in code generation

;; global counter
;; external procedures should never use this
;; variable directly
(define label-counter 0)

(define (new-label-number)
  (set! label-counter (add1 label-counter))
  label-counter)

;; symbol -> symbol
;; make a label (symbol) unique by appending
;; to it a unique number
(define (make-label name)
  (string->symbol
   (string-append
    (symbol->string name)
    (number->string (new-label-number)))))
