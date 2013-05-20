(load "../common/utils.scm")

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination
                   coin-values))
             (cc (- amount
                    (first-denomination
                      coin-values))
                 coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(out (cc 100 us-coins)
     ; 292
     (cc 10 uk-coins)
     ; 50
     (cc 100 (list 1 50 5 25 10))
     ; 292
     )

; order does not effect the answer,
; because the procedure `cc` guarantees that every situation will be taken into account
; for each element in coin-values, there's only 2 choices: do not use it at all/use it for a limited time
; which has nothing to do with list ordering


(end-script)
