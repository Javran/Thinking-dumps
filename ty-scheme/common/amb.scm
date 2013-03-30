(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
      (lambda ()
        (error "amb tree exhausted")))))

(initialize-amb-fail)
; set amb-fail as a lambda

; define amb macro
(define-syntax amb
  (rsc-macro-transformer 
    (let ((xfmr (lambda alts...

                  ; save the previous amb-fail state
                  `(let ((+prev-amb-fail amb-fail))
                     ; resume point #2
                     (call/cc
                       (lambda (+sk)

                         ; for each alternative
                         ,@(map (lambda (alt)
                                  ; resume point #1
                                  `(call/cc
                                     (lambda (+fk)
                                       (set! amb-fail
                                         (lambda ()
                                           ; amb-fail: when called, rollback to the previous fail?
                                           (set! amb-fail +prev-amb-fail)
                                           ; return symbol 'fail'
                                           (+fk 'fail)))
                                       ; return value stored in 'alt'
                                       ; what will happen if the evaluation of 'alt' fails?
                                       ; actually there's no 'error'
                                       ; 'alts..' are either values or sub-ambs,
                                       ; if it's a value, (+sk ,alt) can be evaluated without problem (and return to #2!)
                                       ; if it's an amb, alt is either a value or (amb-fail)
                                       ;     and if (amb-fail) is called, we will go back to resume point #1
                                       ; thus call to '+sk' got cancelled
                                       (+sk ,alt))))
                                alts...)

                         (+prev-amb-fail)))))))

  (lambda (e r)
    (apply xfmr (cdr e))))))
