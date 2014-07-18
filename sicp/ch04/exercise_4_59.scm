(load "./4_4_1_deductive_information_retrieval.scm")

(apply
 qe-asserts!
 '(
   ;; meeting assertions
   (meeting accounting (Monday 9am))
   (meeting administration (Monday 10am))
   (meeting computer (Wednesday 3pm))
   (meeting administration (Friday  1pm))

   (meeting whole-company (Wednesday 4pm))

   (rule (ex4-59-a ?day ?time)
         (meeting ?ignored-1 (?day ?time)))

   (rule (meeting-time ?person ?day-and-time)
         (and (job ?person (?div . ?ignored-1))
              (or (meeting whole-company ?day-and-time)
                  (meeting ?div ?day-and-time))))

   ;; Alyssa arrives at work on Wednesday morning
   ;; and wonders what meetings she has to attend that day.
   (rule (ex4.59-c ?time)
         (meeting-time (Hacker Alyssa P)
                       (Wednesday ?time)))

   ))

(out (qe-all '(ex4-59-a Friday ?time)))
(out (qe-all '(ex4-59-a Wednesday ?time)))

(out (qe-all '(ex4.59-c ?time)))
