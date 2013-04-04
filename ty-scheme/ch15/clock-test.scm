; please use Guile for this source file
(load "../common/guile/utils.scm")

; trap SIGALRM to keep scheme running
(sigaction SIGALRM
           (lambda (sig)
             (display "Signal ")
             (display sig)
             (display " raised. Continuing...")
             (newline)))

; test alarm
(alarm 1)

; the alarm defined above will break the sleep
(sleep 10)
; and "Signal 14 raised ..." will be printed after 1 sec

; the definition below might cause error, use "+inf.0" instead, 
; refer to: http://people.csail.mit.edu/jaffer/III/RAWI
;(define *infinity* (/ 1 0))

(define *infinity* +inf.0)

(out *infinity*)
; test *infinity*
(out (= +inf.0 *infinity*))
; should be #t

(define clock
  (let ((stopped? #t)
        (clock-interrupt-handler
          (lambda () (error "Clock interrupt."))))
    (let ((generate-clock-interupt
            (lambda ()
              (set! stopped? #t)
              (clock-interrupt-handler))))
      (sigaction SIGALRM
                 ; when time up, generate a clock int
                 ;   set "stopped?" as #t
                 ;     invoke int-handler
                 (lambda (sig) (generate-clock-interupt)))
      ; here comes the argument of 'clock'
      (lambda (msg val)
        (case msg
          ((set-handler)
           (set! clock-interrupt-handler val))
          ((set)
           (cond ((= val *infinity*)
                  (let ((time-remaining (alarm 0)))
                    ; return inf if the clock is already stopped
                    (if stopped? *infinity*
                      (begin
                        (set! stopped? #t)
                        time-remaining))))
                 ((= val 0)
                  ; here we'll force int-handler to run

                  (let ((time-remaining (alarm 0)))
                    (if stopped?
                      (begin
                        (generate-clock-interupt)
                        *infinity*)
                      (begin
                        (generate-clock-interupt)
                        time-remaining))))
                 (else

                   (let ((time-remaining (alarm val)))
                     (if stopped?
                       (begin
                         (set! stopped? #f)
                         *infinity*)
                       time-remaining))))))))))
