(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./legacy-easy.scm")

;; I will make the stack a lazy assoc-list
;; because the design of legacy simulator
;; allows registers to be allocated at any time
;; so we shouldn't assume anything about registers

(define (make-stack-alist)
  (cons 'stack-alist '()))

(define (pop-stack-alist stack-alist reg-name)
  (let ((st (cdr (assoc reg-name (cdr stack-alist)))))
    (pop st)))

(define (push-stack-alist stack-alist reg-name val)
  (let ((p (assoc reg-name (cdr stack-alist))))
    (if p
        (let ((st (cdr p)))
          (push st val))
        ;; need to create one
        (let ((st (make-stack)))
          (set-cdr! stack-alist
                    (cons (cons reg-name st)
                          (cdr stack-alist)))
          (push st val)))))

(let ((sa (make-stack-alist)))
  (push-stack-alist sa 'a 30)
  (push-stack-alist sa 'b 20)
  (push-stack-alist sa 'a 10)
  (out (pop-stack-alist sa 'a))
  (out (pop-stack-alist sa 'b))
  (out (pop-stack-alist sa 'a))
)

(end-script)
