(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./qeval.scm")

(load "./exercise_4_75_common.scm")

(apply
 qe-fresh-asserts!
 '(
   (one a)
   (two a)
   (two b)
   ))

(install-handler-unique)

(out (qe-all '(unique (one ?x))))
(out (qe-all '(unique (two ?x))))
(out (qe-all '(unique (no-such ?x))))

(load "./4_4_1_rules.scm")

(install-handler-unique)

(apply
 qe-asserts!
 '(
   ;; should find Ben
   (rule (ex-4-75-a ?x)
         (unique (job ?x (computer wizard))))

   ;; should print the empty stream
   (rule (ex-4-75-b ?x)
         (unique (job ?x (computer programmer))))

   ;; should list all the jobs that are filled by only one person
   (rule (ex-4-75-c ?x ?j)
         (and (job ?x ?j) (unique (job ?anyone ?j))))

   ;; list all people who supervise precisely one person.
   (rule (ex-4-75-d ?sv)
         (and (supervisor ?one ?sv)
              (unique (supervisor ?anyone ?sv))))
   ))

(out (qe-all '(ex-4-75-a ?x)))
(out (qe-all '(ex-4-75-b ?x)))
(out (qe-all '(ex-4-75-c ?x ?j)))
(out (qe-all '(ex-4-75-d ?x)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
