(load "./4_4_1_deductive_information_retrieval.scm")

(apply
 qe-asserts!
 '(
   (rule (lives-near ?person-1 ?person-2)
         (and (address ?person-1 (?town . ?rest-1))
              (address ?person-2 (?town . ?rest-2))
              (not (same ?person-1 ?person-2))))

   (rule (same ?x ?x))

   (rule (wheel ?person)
         (and (supervisor ?middle-manager ?person)
              (supervisor ?x ?middle-manager)))

   (rule (outranked-by ?staff-person ?boss)
         (or (supervisor ?staff-person ?boss)
             (and (supervisor ?staff-person ?middle-manager)
                  (outranked-by ?middle-manager ?boss))))

   ))
