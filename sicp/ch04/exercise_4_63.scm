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

;; find the grandson of Cain
(out (qe-all '(grandson Cain ?x)))
;; find the sons of Lamech
(out (qe-all '(son Lamech ?x)))
;; find the grandsons of Methushael
(out (qe-all '(grandson Methushael ?x)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
