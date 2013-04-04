; please use Guile for this source file
(load "../common/guile/utils.scm")
(load "../common/guile/clock.scm")

(define *engine-escape* #f)
(define *engine-entrance* #f)

(clock 'set-handler
       (lambda ()
         ; pass cc to the engine
         (call/cc *engine-escape*)))

(define make-engine
  ; 'th' is the task, th = thunk
  (lambda (th)
    ; tick-limit, on sucess, on failure
    (lambda (ticks success failure)
      (let* ((ticks-left 0)
             (engine-succeeded? #f)
             (result
               ; resume point #1
               (call/cc
                 (lambda (k)
                   (set! *engine-escape* k)
                   (let ((result
                           ; resume point #2
                           (call/cc
                             (lambda (k)
                               (set! *engine-entrance* k)
                               ; wait for 'ticks' secs
                               (clock 'set ticks)
                               ; run the task,
                               ;   we'll get the result when "(th)" is done
                               ;   but if time is up, *engine-escape* will be called before
                               ;   the computation is done
                               (let ((v (th)))
                                 ; back to resume point #2
                                 ;   and return the result
                                 (*engine-entrance* v))))))
                     (set! ticks-left (clock 'set *infinity*))
                     (set! engine-succeeded? #t)
                     result)))))
        (if engine-succeeded?
          (success result ticks-left)
          (failure
            ; when the computation failed, the continuation(where handler is called) is stored in "result"
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
; (0 10000 20000 ...)
(newline)
(*more* 0.5 list (lambda (ne) (set! *more* ne)))
; resume for another 0.5 sec
(newline)

; quick computation that does not take so long to run
(define quick-computation
  (make-engine
    (lambda ()
      (let loop ((i 0))
        (if (< i 10)
          (begin
            (out i)
            (loop (+ i 1)))
          'done)))))

(define done-report
  (lambda (result ticks-left)
    (out "The computation is done"
         "ticks-left: " ticks-left
         "result: " result)))

(quick-computation 1 done-report (lambda (ne) (set! *more* ne)))
