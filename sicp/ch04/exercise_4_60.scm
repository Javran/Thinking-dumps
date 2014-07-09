(load "./4_4_1_rules.scm")

(apply
 qe-asserts!
 '(
   ;; the reason why the result contains two pairs has been
   ;; explained in "exercise_4_60.pl"
   ;; here I just give the counterpart rule in qeval
   (rule (lives-near2 ?person-1 ?person-2)
         (and (lives-near ?person-1 ?person-2)
              (lisp-value (lambda (a b)
                            (string<? (format #f "~A" a)
                                      (format #f "~A" b)))
                          ?person-1
                          ?person-2)))
   ))


(out (qe-all '(lives-near ?person (Hacker Alyssa P))))
(out (qe-all '(lives-near ?person-1 ?person-2)))
(out (qe-all '(lives-near2 ?person-1 ?person-2)))
