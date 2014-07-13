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
              (son ?w ?s)))

   ))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
