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

; cesaro-test returns a pair: a boolean result and the final random state
(define (cesaro-test rand-update rnd-state)
  (let* ((randoms (gen-random-numbers 2 rand-update rnd-state))
         (value (car randoms))
         (state (cdr randoms))
         (result (= (gcd (car value)
                         (cadr value)) 1)))
    (cons result state)))

; trial-need: the number of single tests needed
; single-test: (single-test rand-update rnd-state) should return a pair:
; the test result and the next random state
(define (make-experiment trial-need single-test)
  ; (experiment state): `s -> (a,s)`
  ; if `s` = nil, the total experiments are done
  ; `a` only makes sense if `s` = nil
  (define (experiment state rand-update)
    (let ((trial-done (car state))
          (trial-success (cadr state))
          (rnd-state (caddr state)))
      (if (= trial-done trial-need)
        (cons (/ trial-success trial-done)
              nil)
        ; run single test, break result into (value,state)
        (let* ((result (single-test rand-update rnd-state))
               (res-value (rnd-state-value result))
               (res-state (rnd-state-state result)))
          (cons 'not-done
                (list
                  (inc trial-done) 
                  (if res-value
                    (inc trial-success)
                    trial-success)
                  res-state))))))
  experiment)

; to write a general monte-carlo is simple:
;   the trial count is hidden inside experiment
;   when each experiment is done, we retrieve the result and break it into
;   the experiment result and the next state
(define (monte-carlo init-state experiment)
  (let* ((exp-result (experiment init-state rand-update))
         (next-value (car exp-result))
         (next-state (cdr exp-result)))
    (if (null? next-state)
      next-value
      (monte-carlo next-state experiment))))

(define (estimate-pi trials)
  (let ((init-state (list 0 0 (rand-update-init)))
        (experiment (make-experiment trials cesaro-test)))
    (sqrt (/ 6 (monte-carlo init-state experiment)))))

(out (estimate-pi 1000))

; ok, I see the point that it's tiresome to write state-passing style
;   repeatly in scheme.

(end-script)
