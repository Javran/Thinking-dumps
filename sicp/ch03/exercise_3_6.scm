(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define random-range 1000)

(define (rand action)
  (define state (make-random-state))
    ;(define (reset new-state)
    ;  (set! state new-state))
  (define (generate)
    (let ((result (random random-range state)))
      (let ((new-state state))
        ;(reset new-state)
        result)))
  (cond ((eq? action 'generate) (generate))
        ((eq? action 'reset) reset)
        (else (error "Unknown message: " m))))

(define (generate-numbers count)
  (if (= count 0)
    nil
    (cons (rand 'generate)
          (generate-numbers (dec count)))))

(out (generate-numbers 10))

(end-script)
