(load "../common/utils.scm")
(load "../common/test-utils.scm")

; missing definitions:
; * (make-connector)
; * (set-value! var value source)
; * (forget-value! var source)
; * (connect x y)
; * (get-value c)

(define (make-connector)
  (let ((value #f)
        (informant #f)
        (constraints nil))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
            ; if I don't have any value
              (set! value newval)
              (set! informant setter)
              ; inform related constraints that something has changed
              (for-each-except setter
                               inform-about-value
                               constraints))
            ((not (= value newval))
            ; if I have a value, which does not equal to
            ;   the newval
              (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      ; why should we check the retractor?
      (if (eq? retractor informant)
        (begin (set! informant #f)
               (for-each-except retractor
                                inform-about-no-value
                                constraints))
        'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints
          (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
              (if informant #t #f))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation: CONNECTOR"
                         request))))
    me))

(define (for-each-except exception proc ls)
  (define (run-it x) (proc x))
  (define (not-exception? a) (not (eq? a exception)))
  (for-each
    run-it
    (filter not-exception? ls)))

(end-script)
