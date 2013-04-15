(load "../common/utils.scm")

(define result
  (/ 
    (+ 5
       4
       (- 2
          (- 3 
             (+ 6 
                (/ 4 5)))))
    (* 3 (- 6 2) (- 2 7))))

(out 
  result
  (exact->inexact result))

; verify it:
; dc -e "7k 5 4 + 2 3 6 4 5/+--+ 3 6 2 -* 2 7 -*/p "
; the result should be -.2466666
