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
  ; note here we don't have any parameter
  ;   because all we need is just a signal
  ;   that indicates if thers's any change
  ;   on related connectors
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

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
            (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
            (set-value! product
                        (* (get-value m1) (get-value m2))
                        me))
          ((and (has-value? product) (has-value? m1))
            (set-value! m2
                        (/ (get-value product)
                           (get-value m1))
                        me))
          ((and (has-value? product) (has-value? m2))
            (set-value! m1
                        (/ (get-value product)
                           (get-value m2))
                        me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
            (process-new-value))
          ((eq? request 'I-lost-my-value)
            (process-forget-value))
          (else (error "Unknown request: MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (format #t
      "Probe: ~A = ~A~%"
      name value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
            (process-new-value))
          ((eq? request 'I-lost-my-value)
            (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  (connect connector me))

; missing definitions:
; * (make-connector)
; * (set-value! var value source)
; * (forget-value! var source)
; * (connect x y)
; * (get-value c)

(end-script)
