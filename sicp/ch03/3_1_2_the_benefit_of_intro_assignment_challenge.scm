(load "../common/utils.scm")
(load "../common/test-utils.scm")

; this source code tries to challenge the statement in book
;   that `makes it difficult for us to isolate the Monte
;   Carlo idea`.

(define make-rnd-state cons)
(define rnd-state-result car)
(define rnd-state-state cdr)

(define (cesaro-test rand-update rnd-state)
  (let* ((rs1 (rand-update rnd-state))
         (rs2 (rand-update (rnd-state-state rs1)))
         (r1 (rnd-state-result rs1))
         (r2 (rnd-state-result rs2))
         (fin-state (rnd-state-state rs2))
         (result (= (gcd r1 r2) 1)))
    (make-rnd-state result fin-state)))

; trial-need: the number of single tests needed
; single-test: (single-test rand-update rnd-state) should return a pair:
; the test result and the next random state
(define (make-experiment trial-need single-test)
  ; (experiment state): s -> (a,s)
  ; if s = nil, the total experiments are done
  (define (experiment state rand-update)
    (let ((trial-done (car state))
          (trial-success (cadr state))
          (rnd-state (caddr state)))
      (if (= trial-done trial-need)
        (cons (/ trial-success trial-done)
              nil)
        (let* ((result (single-test rand-update rnd-state))
               (res-value (rnd-state-result result))
               (res-state (rnd-state-state result)))
          (if res-value
            (cons #t
          


(end-script)
