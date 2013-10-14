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

(end-script)
