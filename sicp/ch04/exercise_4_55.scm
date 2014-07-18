(load "./4_4_1_deductive_information_retrieval.scm")

;; all people supervised by Ben Bitdiddle
(out (qe-all '(supervisor ?x (Bitdiddle Ben))))

;; the names and jobs of all people in the accounting division
(out (qe-all '(job ?name (accounting . ?rest))))

;; the names and addresses of all people who live in Slumerville
(out (qe-all '(address ?name (Slumerville . ?rest))))
