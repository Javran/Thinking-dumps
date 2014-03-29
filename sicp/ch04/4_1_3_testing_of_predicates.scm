(load "../common/utils.scm")
(load "../common/test-utils.scm")

; I personally think this makes little sense
;   and might cause some confusion. 
; Actually this shows that the implementing language
;   and the implemented language would have a different set
;   of data structure or type that they can manipulate.
; Given that this would be a metacircular evaluator,
;   we don't have to do this.

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

(end-script)
