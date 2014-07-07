(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./qeval.scm")

(apply
 qe-fresh-asserts!
 '(;; Ben Bitdiddle
   (address (Bitdiddle Ben)
            (Slumerville (Ridge Road) 10))
   (job (Bitdiddle Ben)
        (computer wizard))
   (salary (Bitdiddle Ben) 60000)
   (supervisor (Bitdiddle Ben)
               (Warbucks Oliver))

   ;; Alyssa P. Hacker
   (address (Hacker Alyssa P)
            (Cambridge (Mass Ave) 78))
   (job (Hacker Alyssa P)
        (computer programmer))
   (salary (Hacker Alyssa P) 40000)
   (supervisor (Hacker Alyssa P)
               (Bitdiddle Ben))

   ;; Cy D. Fect
   (address (Fect Cy D)
            (Cambridge (Ames Street) 3))
   (job (Fect Cy D)
        (computer programmer))
   (salary (Fect Cy D) 35000)
   (supervisor (Fect Cy D)
               (Bitdiddle Ben))

   ;; Lem E. Tweakit
   (address (Tweakit Lem E)
            (Boston (Bay State Road) 22))
   (job (Tweakit Lem E) (computer technician))
   (salary (Tweakit Lem E) 25000)
   (supervisor (Tweakit Lem E)
               (Bitdiddle Ben))

   ;; Louis Reasoner
   (address (Reasoner Louis)
            (Slumerville (Pine Tree Road) 80))
   (job (Reasoner Louis) (computer programmer trainee))
   (salary (Reasoner Louis) 30000)
   (supervisor (Reasoner Louis)
               (Hacker Alyssa P))

   ;; Oliver Warbucks
   (address (Warbucks Oliver)
            (Swellesley (Top Heap Road)))
   (job (Warbucks Oliver) (administration big wheel))
   (salary (Warbucks Oliver) 150000)

   ;; Eben Scrooge
   (address (Scrooge Eben) (Weston (Shady Lane) 10))
   (job (Scrooge Eben) (accounting chief accountant))
   (salary (Scrooge Eben) 75000)
   (supervisor (Scrooge Eben) (Warbucks Oliver))

   ;; Robert Cratchet
   (address (Cratchet Robert)
            (Allston (N Harvard Street) 16))
   (job (Cratchet Robert) (accounting scrivener))
   (salary (Cratchet Robert) 18000)
   (supervisor (Cratchet Robert) (Scrooge Eben))

   ;; DeWitt Aull
   (address (Aull DeWitt)
            (Slumerville (Onion Square) 5))
   (job (Aull DeWitt) (administration secretary))
   (salary (Aull DeWitt) 25000)
   (supervisor (Aull DeWitt)
               (Warbucks Oliver))

   )

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
