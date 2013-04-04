; please use Guile for this source file
(load "../common/guile/utils.scm")
(load "../common/guile/clock.scm")

(define *engine-escape* #f)
(define *engine-entrance* #f)

(clock 'set-handler
       (lambda ()
         (call/cc *engine-escape*)))

(define make-engine
  (lambda (th)
    (lambda (ticks success failure)
      (let* ((ticks-left 0)
             (engine-succeeded? #f)
             (result
               (call/cc
                 (lambda (k)
                   (set! *engine-escape* k)
                   (let ((result
                           (call/cc
                             (lambda (k)
                               (set! *engine-entrance* k)
                               (clock 'set ticks)
                               (let ((v (th)))
                                 (*engine-entrance* v))))))
                     (set! ticks-left (clock 'set *infinity*))
                     (set! engine-succeeded? #t)
                     result)))))
        (if engine-succeeded?
          (success result ticks-left)
          (failure
            (make-engine
              (lambda ()
                (result 'resume)))))))))

; test engine
(define printn-engine
  (make-engine
    (lambda ()
      (let loop ((i 0))
        (if (= 0 (remainder i 10000))
          (begin 
            (display i)
            (display " ")))
        (loop (+ i 1))))))

(define *more* #f)
(printn-engine 0.5 list (lambda (ne) (set! *more* ne)))
