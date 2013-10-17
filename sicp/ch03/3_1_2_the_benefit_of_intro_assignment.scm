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

(define random-range 1000000)
(define trial-count 1000)

(let ()
  (define (rand) (random random-range))

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

  (out (estimate-pi trial-count)))

(let ()
  ; try to separate generated number and the internal state
  ; call `rand-result` on a `pair` object to get the result
  ; call `rand-state` on a `pair` object to get the random state
  (define rand-result car)
  (define rand-state cdr)
  (define mk-rand-st cons)

  (define (rand-update pair)
    (let ((st (rand-state pair)))
      ; since random-state object get updated when called by random
      ;   simply pass the old one should be ok
      (mk-rand-st (random random-range st)
                  st)))

  (define random-init (mk-rand-st nil (make-random-state)))
  
  (define (estimate-pi trials)
    (sqrt (/ 6 (random-gcd-test trials random-init))))

  (define (random-gcd-test trials initial-x)
    (define (iter trials-remaining trials-passed x)
      (let ((x1 (rand-update x)))
        (let ((x2 (rand-update x1)))
          (cond ((= trials-remaining 0)
                  (/ trials-passed trials))
                ((= (gcd (rand-result x1) (rand-result x2)) 1)
                  (iter (- trials-remaining 1)
                        (+ trials-passed 1)
                        x2))
                (else
                  (iter (- trials-remaining 1)
                        trials-passed
                        x2))))))
    (iter trials 0 initial-x))
  (out (estimate-pi trial-count)))

; yes, I don't think monte carlo cannot be abstract simply because
;   we had to handle random-states explicitly.
;   extra task: impl monte-carlo in a way that can be used without
;   knowing the experiment details

(end-script)
