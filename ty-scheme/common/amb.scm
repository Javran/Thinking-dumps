(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
      (lambda ()
        (error "amb tree exhausted")))))

(initialize-amb-fail)

(define-syntax amb
  (rsc-macro-transformer 
    (let ((xfmr (lambda alts...

                  `(let ((+prev-amb-fail amb-fail))
                     (call/cc
                       (lambda (+sk)

                         ,@(map (lambda (alt)
                                  `(call/cc
                                     (lambda (+fk)
                                       (set! amb-fail
                                         (lambda ()
                                           (set! amb-fail +prev-amb-fail)
                                           (+fk 'fail)))
                                       (+sk ,alt))))
                                alts...)

                         (+prev-amb-fail)))))))


  (lambda (e r)
    (apply xfmr (cdr e))))))
