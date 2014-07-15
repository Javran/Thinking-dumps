(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./qeval.scm")

;; consider the following example:
;; `inf-list` will generate an infinite stream of lists
;; containing only symbol `a` and `b`.
;; there are two rules that insert `a` or `b` to the front
;; of an existing valid list. if we simply append two streams
;; together, because the first stream is infinite, the second
;; rule will not have a chance to be applied.
;; but if we interleave the streams, then both stream
;; will eventually have their chances to be applied
;; despite that the first stream might be infinite.

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
      (stream-take
       10
       (qe-stream '(inf-list ?a)))))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
