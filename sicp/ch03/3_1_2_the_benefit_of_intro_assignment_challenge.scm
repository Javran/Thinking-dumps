(load "../common/utils.scm")
(load "../common/test-utils.scm")

; this source code tries to challenge the statement in book
;   that `makes it difficult for us to isolate the Monte
;   Carlo idea`.

(define random-range 1000000)
(define make-rnd-state cons)
(define rnd-state-value car)
(define rnd-state-state cdr)

; generate a list of random numbers of size len,
;   and return (a,s) where a is the list and s is the final state
(define (gen-random-numbers len rand-update init-state)
  (if (= 0 len)
    (cons '() init-state)
    (let* ((rnd-obj (rand-update init-state))
           (value (rnd-state-value rnd-obj))
           (state (rnd-state-state rnd-obj))
           (rest-objs (gen-random-numbers (dec len) rand-update state))
           (rest-values (car rest-objs))
           (fin-state (cdr rest-objs)))
      (cons (cons value rest-values)
            fin-state))))

(define make-ru cons)
(define ru-value car)
(define ru-state cdr)

(define rand-update-init make-random-state)
(define (rand-update st)
  (let* ((st1 (make-random-state st))
         (value (random random-range st1)))
    (make-ru value st1)))

(out (gen-random-numbers 10 rand-update (rand-update-init)))

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
          (cons #t
                (list
                  (inc trial-done) 
                  ((if res-value inc identity) trial-success)
                  res-state))))))
  experiment)

(end-script)
