(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define random-range 1000)

; don't use (define (rand <args>) ...) here
;   because every time we call it, it will generate a new
;   environment that we're unable to use
;   so instead, we write a `dispatch` so the inside environment
;   can be persistently available
(define rand
  (let ((state nil))
    (define (reset new-state)
      ; make a copy so this generator will not change
      ;   the state passed in
      (set! state (make-random-state new-state))
      'done)
    (define (generate)
      (if (null? state)
        (error "rand state not initialized")
        'pass)
      (let ((result (random random-range state)))
        ; `random` has side-effect on `state`
        ;   simply assign `state` to itself
        (reset state)
        result))
    (define (dispatch action)
      (cond ((eq? action 'generate) (generate))
            ((eq? action 'reset) reset)
            (else (error "Unknown action: " action))))
    dispatch))

(define (generate-numbers count)
  (if (= count 0)
    nil
    (cons (rand 'generate)
          (generate-numbers (dec count)))))

(define state (make-random-state #t))

((rand 'reset) state)

(define result-1 (generate-numbers 10))
(define result-2 (generate-numbers 10))

((rand 'reset) state)
(define result-3 (generate-numbers 10))

(out result-1 result-2 result-3)
(assert (equal? result-1 result-3))
(out "random generator with state - test passed")

(end-script)
