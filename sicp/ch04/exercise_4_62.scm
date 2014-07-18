(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./qeval.scm")

(apply
 qe-fresh-asserts!
 '(
   (rule (same ?x ?x))
   ;; to make it work even on improper list,
   ;; we need to say that the "cdr" part cannot
   ;; be unified with another pair pattern.
   (rule (last-pair (?x . ?y) (?x . ?y))
         (not (same ?y (?y1 . ?y2))))
   (rule (last-pair (?x . ?y) ?z)
         (last-pair ?y ?z))

   ))

(out (qe-all '(last-pair (3) ?x)))
(out (qe-all '(last-pair (1 2 3) ?x)))
(out (qe-all '(last-pair (2 ?x) (3))))
(out (qe-all '(last-pair (a b c . d) ?x)))

;; the following line
;; won't work despite that there are lots of solutions
;; we just cannot give a "stream of possible solutions"
;; and then filter out invalid solutions.
;; (out (qe-all '(last-pair ?x (3))))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
