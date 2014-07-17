(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./qeval.scm")

#|
(apply
 qe-fresh-asserts!
 '(
   (edge a b)
   (edge b c)
   (edge c d)

   ;; wrong rule
   (rule (link ?x ?y) (edge ?x ?y))
   (rule (link ?x ?z)
         (and (link ?y ?z)
              (edge ?x ?y)))

   ;; correct one
   (rule (link2 ?x ?y) (edge ?x ?y))
   (rule (link2 ?x ?z)
         (and (edge ?x ?y)
              (link2 ?y ?z)))

   ))

(out (qe-all '(link a d)))

;; from: 4_4_1_rules.scm
;; "outranked-by" modified according to exercise 4.64.
(load "./4_4_1_deductive_information_retrieval.scm")

(apply
 qe-asserts!
 '(
   (rule (lives-near ?person-1 ?person-2)
         (and (address ?person-1 (?town . ?rest-1))
              (address ?person-2 (?town . ?rest-2))
              (not (same ?person-1 ?person-2))))

   (rule (same ?x ?x))

   (rule (wheel ?person)
         (and (supervisor ?middle-manager ?person)
              (supervisor ?x ?middle-manager)))

   (rule (outranked-by ?staff-person ?boss)
         (or (supervisor ?staff-person ?boss)
             (and (outranked-by ?middle-manager ?boss)
                  (supervisor ?staff-person ?middle-manager))))

   ))

(qe-all '(outranked-by (Bitdiddle Ben) ?who))

|#

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
