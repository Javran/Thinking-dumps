(load "../common/utils.scm")
(load "../common/test-utils.scm")

;; well, let's begin with some experiment:
;; say that we have 1,2,3 as numbers,
;; and the goal is to find all "less-than" pairs:
;; (1,2) (1,3) (2,3)

(load "./qeval.scm")

(apply
 qe-fresh-asserts!
 '(
   (num 1)
   (num 2)
   (num 3)

   (rule (less-than ?a ?b)
         (and (num ?a)
              (num ?b)
              (lisp-value < ?a ?b)))
   ))

(out (qe-all '(less-than ?a ?b)))

(load "./amb-eval.scm")

(out (amb-eval-all
      `(let ((num '(1 2 3)))
         (let ((a (an-element-of num))
               (b (an-element-of num)))
           (require (< a b))
           (cons a b)))
      (amb-init-env)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
