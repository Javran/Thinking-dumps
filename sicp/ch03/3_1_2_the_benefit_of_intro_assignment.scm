(load "../common/utils.scm")
(load "../common/test-utils.scm")

; here I give some examples regarding
;   mit-scheme's random implementation
; related document:
; http://www.gnu.org/software/mit-scheme/documentation/mit-scheme-ref/Random-Numbers.html

(define (rand-impl-example)
  ; using the same random-state
  (define seed1 (make-random-state))
  (define seed2 (make-random-state seed1))

  (define (gen-seq seed)
    (map (lambda (x)
           (random 1000 seed))
         (list-in-range 1 10)))
  ; these two output should be exactly the same
  (out (gen-seq seed1))
  (out (gen-seq seed2))
  ; however, the seed itself got modified(side-effect of `random`
  ; for example:
  (out (random 1000 seed1)
       (random 1000 seed1))
  ; we cannot guarantee that these two `(random seed1)` will produce the same outcome
  (newline)
  )

(rand-impl-example)

(let ()
  (define (rand) (random 1000000))

  (define (estimate-pi trials)
    (sqrt (/ 6 (monte-carlo trials cesaro-test))))
  (define (cesaro-test)
    (= (gcd (rand) (rand)) 1))

  (define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
      (cond ((= trials-remaining 0)
              (/ trials-passed trials))
            ((experiment)
              (iter (- trials-remaining 1)
                    (+ trials-passed 1)))
            (else
              (iter (- trials-remaining 1)
                    trials-passed))))
    (iter trials 0))

  (out (estimate-pi 1000))
  )

(end-script)
