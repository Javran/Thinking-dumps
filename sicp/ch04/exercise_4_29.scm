(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; function memoization
(define (memoize f)
  (let ((retval-alist nil))
    (lambda args
      (cond ((assoc args retval-alist) =>
             cadr)
            (else
             (let ((result (apply f args)))
               (set! retval-alist
                     (cons (list args result)
                           retval-alist))
               result))))))

;; Exhibit a program that runs more slowly
;; without memoization

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
