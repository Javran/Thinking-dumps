(load "../common/utils.scm")
(load "../common/test-utils.scm")

; I want to summarize all the previous work done
; in this chapter, make `my-eval`, and expand it
; throughout this chapter.
(load "./my-eval.scm")

(out (string? (my-eval '(quote "a") #f)))

(end-script)
