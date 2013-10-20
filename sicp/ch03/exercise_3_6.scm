(load "../common/utils.scm")
(load "../common/test-utils.scm")

(define random-range 1000)

(define (rand action)
  (define state nil) 
  (define (reset new-state)
    (set! state new-state)
    (pp state)
    'done)
  (define (generate)
    (let ((result (random random-range state)))
      ; `random` has side-effect on `state`
      ;   simply assign `state` to itself
      ;(reset state)
      result))
  (cond ((eq? action 'generate) (generate))
        ((eq? action 'reset) reset)
        (else (error "Unknown action: " action))))

(define (generate-numbers count)
  (if (= count 0)
    nil
    (cons (rand 'generate)
          (generate-numbers (dec count)))))

(out ((rand 'reset) (make-random-state #f)))

(out (rand 'generate))
(out (rand 'generate))
(out (rand 'generate))

(end-script)
