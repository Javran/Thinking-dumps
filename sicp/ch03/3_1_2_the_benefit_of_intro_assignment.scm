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
  )

(rand-impl-example)


(end-script)
