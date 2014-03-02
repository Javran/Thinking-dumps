(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; it's confusing here what the "memoization"
;; refers to, it could be "function memoization" or
;; "thunk memoization".

(define (thunk-no-memo exp env)
  (list 'thunk-nm
        (lambda ()
          (eval exp env))))

(define (thunk-memo exp env)
  (let ((data (list 'thunk-m 'unused)))
    (set-car! (cdr data)
              (lambda ()
                (let ((result (eval exp env)))
                  (set-car! data 'thunk-mv)
                  (set-car! (cdr data) result)
                  result)))
    data))

;; force a thunk,
;; if variavle `thunk` is not a thunk,
;; just return it.
(define (force-th thunk)
  (if (non-empty? thunk)
      (case (car thunk)
        ((thunk-nm thunk-m) ((cadr thunk)))
        (else (cadr thunk)))
      thunk))

(define env user-initial-environment)

;; Exhibit a program that runs much more slowly
;; without memoization

;; a simple example would be just calling
;; a costly function for multiple times
(define (gen-costly-thunk thunk-xxx)
  (thunk-xxx
  `(fold-left + 0 '(1 1000))
  env))

(define (test-thunk-maybe-memo thunk)
  (for-each
   (lambda (t)
     (force-th t))
   (map (const thunk)
        (list-in-range 1 1000))))

(time-test test-thunk-maybe-memo (gen-costly-thunk thunk-memo))
(time-test test-thunk-maybe-memo (gen-costly-thunk thunk-no-memo))

;; here we expect the version without memoization
;; to take a significantly longer time to run

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
