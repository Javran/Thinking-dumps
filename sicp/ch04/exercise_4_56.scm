(load "./4_4_1_deductive_information_retrieval.scm")

(apply
 qe-asserts!
 '(
   ;; the names of all people who are supervised by
   ;; Ben Bitdiddle, together with their addresses
   (rule (ex4.56-a ?name ?addr)
         (and (supervisor ?name (Bitdiddle Ben))
              (address ?name ?addr)))

   ;; all people whose salary is less then Ben Bitdiddle's,
   ;; together with their salary and Ben Bitdiddle's salary
   (rule (ex4.56-b ?name ?sal ?bsal)
         (and (salary (Bitdiddle Ben) ?bsal)
              (salary ?name ?sal)
              (lisp-value < ?sal ?bsal)))

   ;; all people who are supervised by someone who is not
   ;; in the computer division, together with the supervisor's
   ;; name and job
   (rule (ex4.56-c ?name ?sname ?sjob)
         (and (supervisor ?name ?sname)
              (job ?sname ?sjob)
              (not (job ?sname (computer . ?sjrest)))))
   ))

(out (qe-all '(ex4.56-a ?name ?addr)))
(out (qe-all '(ex4.56-b ?n ?s ?sb)))
(out (qe-all '(ex4.56-c ?n ?sn ?sj)))
