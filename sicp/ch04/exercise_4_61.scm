(load "./4_4_1_rules.scm")

(apply
 qe-asserts!
 '(
   (rule (?x next-to ?y in (?x ?y . ?u)))
   (rule (?x next-to ?y in (?v . ?z))
         (?x next-to ?y in ?z))
   ))

(out (qe-all '(?x next-to ?y in (1 (2 3) 4))))
;; the response should be:
;; * (1 next-to (2 3) in (1 (2 3) 4))
;; * ((2 3) next-to 4 in (1 (2 3) 4))
(out (qe-all '(?x next-to 1 in (2 1 3 1))))
;; the response should be:
;; * (2 next-to 1 in (2 1 3 1))
;; * (3 next-to 1 in (2 1 3 1))
