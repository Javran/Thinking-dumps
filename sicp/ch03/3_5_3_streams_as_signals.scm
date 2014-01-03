(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./stream.scm")

(define (integral integrand init-val dt)
  (define int
    (cons-stream
      init-val
      (add-streams
        (scale-stream integrand dt)
        int)))
  int)

(run-stream-test)
; todo: verify that:
; cos x = 1 - int(sin x dx)
; cos x = int( -sinx dx) + 1

(define (estimate-cos upbound eps)
  (define range
    (scale-stream integers eps))
  (define neg-sins
    ; x -> - sin x
    (stream-map
      (compose - sin)
      range))
  (define the-int
    (integral neg-sins 1 eps))
  (define estimated-val 
    (cdr
      (head
        ; drop until we've reached the upbound
        (drop-while
          (lambda (pair)
            ; pair: (cons <x> < -sin x >)
            (<= (car pair) upbound))
          ((zip-streams-with cons)
           range
           the-int)))))
  (format
    #t
    "x         = ~A~%~
     cos x     = ~A~%~
     eps       = ~A~%~
     estimated = ~A~%~
     diff      = ~A~%"
    upbound
    (cos upbound)
    eps
    estimated-val
    (abs (- (cos upbound) estimated-val))))

(estimate-cos 0.25 1e-6)

(end-script)
