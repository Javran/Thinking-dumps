(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define random-range 1000)

(define (rand action)
  (let ((state (make-random-state)))
    (define (reset new-state)
      (set! state new-state))
    (define (generate)
      (let ((result (random random-range state)))
        (let ((new-state state))
          (reset new-state)
          result)))
    (define (dispatch m)
      (cond ((eq? m 'generate) (generate))
            ((eq? m 'reset) reset)
            (else (error "Unknown message: " m))))
    (dispatch action)))



(end-script)
