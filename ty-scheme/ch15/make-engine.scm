(load "../common/utils.scm")

; how to make it work, we don't have clock

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


