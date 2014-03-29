(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define (make-monitored f)
  (let ((counter 0))
    (define (call-f . args)
      (if (= (length args) 1)
        (let ((cmd (car args)))
          (cond ((eq? cmd 'how-many-calls?)
                  counter)
                ((eq? cmd 'reset-count)
                  (set! counter 0)
                  counter)
                (else
                  (set! counter (inc counter))
                  (apply f args))))
        (begin
          (set! counter (inc counter))
          (apply f args))))
    call-f))

(define s (make-monitored sqrt))
(out (s 'how-many-calls?))
; 0
(out (s 100))
; 10
(out (s 'how-many-calls?))
; 1

(newline)
(define m+ (make-monitored +))
(out (m+ 1 2 3 4))
; 10
(out (m+ 5 5))
; 10
(out (m+ 'how-many-calls?))
; 2
(m+ 'reset-count)
(out (m+ 1 3 5 7))
; 16
(out (m+ 'how-many-calls?))
; 1

(end-script)
