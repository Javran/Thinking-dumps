(load "./4_4_1_deductive_information_retrieval.scm")

(apply
 qe-asserts!
 '(
   ;; a person is a "big shot" in a division
   ;; if the person works in the division but
   ;; does not have a supervisor who works
   ;; in the division
   (rule (big-shot ?n)
         (and (supervisor ?n ?ns)
              (job ?n (?div . ?ignored-1))
              (job ?ns (?divs . ?ignored-2))
              (lisp-value (lambda (x y) (not (equal? x y)))
                          ?div ?divs)))
   ))

(out (qe-all '(big-shot ?n)))

