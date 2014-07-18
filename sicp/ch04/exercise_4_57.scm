(load "./4_4_1_deductive_information_retrieval.scm")

(apply
 qe-asserts!
 '(
   (rule (can-replace ?a ?b)
         ;; saying that "?a can replace ?b"
         ;; or "?b can be replaced by ?a"
         (and
          (or (and (job ?a ?job)
                   (job ?b ?job))
              (and (job ?a ?joba)
                   (job ?b ?jobb)
                   (can-do-job ?joba ?jobb)))
          (not (lisp-value equal? ?a ?b))))

   ;; all people who can replace Cy D. Fect
   (rule (ex-4.57-a ?name)
         (can-replace ?name (Fect Cy D)))

   ;; all people who can replace someone who
   ;; is being paid more than they are
   ;; together with
   (rule (ex-4.57-b ?na ?sa ?nb ?sb)
         (and (salary ?na ?sa)
              (salary ?nb ?sb)
              (can-replace ?na ?nb)
              (lisp-value < ?sa ?sb)))
   ))

(out (qe-all '(ex-4.57-a ?n)))
(out (qe-all '(ex-4.57-b ?na ?sa ?nb ?sb)))
