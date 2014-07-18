(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./qeval.scm")

(apply
 qe-fresh-asserts!
 '(
   ;; (append-to-form ?a ?b ?c) satisfies:
   ;; ?a ++ ?b == ?c
   (rule (append-to-form () ?y ?y))
   (rule (append-to-form (?u . ?v) ?y (?u . ?z))
         (append-to-form ?v ?y ?z))

   (rule (reverse () ()))
   (rule (reverse (?u . ?v) ?z)
         (and (reverse ?v ?rev-v)
              (append-to-form ?rev-v (?u) ?z)))

   ))

;; example
(out (qe-all '(append-to-form ?x (3 4 5) (1 2 3 4 5))))

(out (qe-all '(reverse (1 2 3 4) ?x)))
;; I don't think `(reverse ?x (1 2 3)) will ever work.
;; since we don't perform pattern on the second argument part of the query.

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
