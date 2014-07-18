(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./qeval.scm")

(apply
 qe-fresh-asserts!
 '(
   (infinite a)

   (rule (infinite ?a)
         (infinite ?a))))

(out (stream->list (stream-take 4 (qe-stream '(infinite ?x)))))

;; note here the rule "infinite" calls itself and which results in an infinite loop.
;; trace: disjoin handler -> qeval -> disjoin handler -> ...
;; so if the code is written in the same manner that code in exercise 4.71 is written,
;; the program above will result in an infinite loop not printing out anything
;; despite that we are taking elements from a stream.

;; previous what confuses me most is that I thought since "disjoin" handler
;; will make a stream, there is no reason to use "delay" anyway. But
;; the point is that it's not the infinite stream that causes the infinite loop,
;; but the actual problem lies in the process of making the stream,
;; rather than the stream itself.

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
