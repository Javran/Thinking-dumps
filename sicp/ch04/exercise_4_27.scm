(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; please also refer to `./exercise 4.27.md`
;; for some explanation.
;; Here I just implement lazy evaluation using
;; scheme's native support and verify the result.

(define count 0)
(define (id x)
  ;;  x is delayed here
  (set! count (+ count 1))
  ;; by observation that `id` is non-strict in `x`
  x)

(define w (id (delay (id (delay 10)))))

(define (actual x)
  (if (promise? x)
      (actual (force x))
      x))

(out (actual count))
(out (actual w))
(out (actual count))

;; 1
;; 10
;; 2

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
