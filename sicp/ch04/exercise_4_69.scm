(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./qeval.scm")

(apply
 qe-fresh-asserts!
 '(
   ;; (son ?x ?y):
   ;; * ?x's son is ?y
   ;; * ?y is the son of ?x
   ;; same thing for "wife"
   (son Adam Cain)
   (son Cain Enoch)
   (son Enoch Irad)
   (son Irad Mehujael)
   (son Mehujael Methushael)
   (son Methushael Lamech)
   (wife Lamech Ada)
   (son Ada Jabal)
   (son Ada Jubal)

   (rule (grandson ?g ?s)
         (and (son ?f ?s)
              (son ?g ?f)))

   (rule (son ?m ?s)
         (and (wife ?m ?w)
              (son ?w ?s)))  (rule (same ?x ?x))

   (rule (last-pair (?x . ?y) (?x . ?y))
         (not (same ?y (?y1 . ?y2))))
   (rule (last-pair (?x . ?y) ?z)
         (last-pair ?y ?z))

   (rule ((grandson) ?x ?y)
         (grandson ?x ?y))

   (rule ((great . ?rel) ?x ?y)
         (and (son ?x ?z)
              (?rel ?z ?y)
              (last-pair ?rel (grandson))))

   ))

(out (qe-all '((great grandson) ?g ?ggs)))
(out (qe-all '(?relationship Adam Irad)))
(out (qe-all '((great great great great great grandson) ?g ?ggs)))
(out (qe-all '(?rel Adam Jubal)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
