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

;; now we need to figure out how to "append" in amb...

(apply
 qe-fresh-asserts!

 '(
   (rule (append () ?b ?b))

   (rule (append (?ah . ?al) ?b (?ah . ?c))
         (append ?al ?b ?c))

   ))

(out (qe-all '(append (a b c) (d e f) ?x)))

;; a more basic case will be: how to do pattern matching
;; and unification in amb?

(apply
 qe-fresh-asserts!

 '(
   (pat (a b c))
   (pat (d e))
   ))

(out (qe-all '(pat (?a . ?b))))
(out (qe-all '(pat (?a ?b))))

;; TODO:
;; I believe solving these questions would help somehow,
;; but now I don't have a clear idea about how to implement this
;; Maybe I'll skip these questions and move to chapter 5.
;; Will come back later.

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
