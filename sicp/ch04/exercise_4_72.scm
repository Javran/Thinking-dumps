(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./qeval.scm")

(apply
 qe-fresh-asserts!
 '(
   (inf-list ())

   (rule (same ?a ?a))

   (rule (inf-list (a . ?a))
         (inf-list ?a))

   (rule (inf-list (b . ?a))
         (inf-list ?a))
   ))

(out (stream->list
      (stream-take 10
                   (qe-stream '(inf-list ?a)))))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
