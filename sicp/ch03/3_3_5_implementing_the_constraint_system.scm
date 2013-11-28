(load "../common/utils.scm")
(load "../common/test-utils.scm")

; abstraction:
; * (has-value? <connector>)
; * (get-value <connector>)
; * (set-value! <connector> <new-value> <informant>)
; * (forget-value! <connector> <retractor>)
; * (connect <connector> <new-constraint>)
; * (inform-about-value <constraint>)
; * (inform-about-no-value <constraint>)

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
          ; a1, a2 -> sum
            (set-value! sum
                        (+ (get-value a1)
                           (get-value a2))
                        me))
          ((and (has-value? a1) (has-value? sum))
          ; a1, sum -> a2
            (set-value! a2
                        (- (get-value sum)
                           (get-value a1))
                        me))
          ((and (has-value? a2) (has-value? sum))
          ; a2, sum -> a1
            (set-value! a1
                        (- (get-value sum)
                           (get-value a2))
                        me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
            (process-new-value))
          ((eq? request 'I-lost-my-value)
            (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-no-value constraint)
  (constraint 'I-have-a-value))

(define (inform-about-no-value contraint)
  (constraint 'I-lost-my-value))

; missing definitions:
; * (make-connector)
; * (multiplier a b c)
; * (adder a b c)
; * (constant a b)
; * (probe str var)
; * (set-value! var value source)
; * (forget-value! var source)

(end-script)
