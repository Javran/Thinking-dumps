(load "../common/utils.scm")
(load "../common/test-utils.scm")

; I'll use the same notation used in "./3_2_2_applying_simple_procedures.scm"

(define (factorial-rec n)
  (if (= n 1) 1 (* n (factorial-rec (- n 1)))))

(out (factorial-rec 6))
; (factorial-rec 6) : G
; (if (= n 1) 1 (* n (factorial-rec (- n 1)))) : E1:[G], n = 6 in E1
; (* n (factorial-rec (- n 1))) : E1:[G]
;   (if (= n 1) 1 (* n (factorial-rec (- n 1)))) : E2:[E1,G], n = 5 in E2
;   (* n (factorial-rec (- n 1))) : E2:[E1,G]
;     (if (= n 1) 1 (* n (factorial-rec (- n 1)))) : E3:[E2,E1,G], n = 4 in E3
;     (* n (factorial-rec (- n 1))) : E3:[E2,E1,G]
;       (if (= n 1) 1 (* n (factorial-rec (- n 1)))) : E4:[E3,E2,E1,G], n = 3 in E4
;       (* n (factorial-rec (- n 1))) : E4:[E3,E2,E1,G]
;         (if (= n 1) 1 (* n (factorial-rec (- n 1)))) : E5:[E4,E3,E2,E1,G], n = 2 in E5
;         (* n (factorial-rec (- n 1))) : E5:[E4,E3,E2,E1,G]
;           (if (= n 1) 1 _) : E6:[E5,E4,E3,E2,E1,G], n = 1 in E6
;           1 : E6:[E5,E4,E3,E2,E1,G]
;         (* n 1) E5:[E4,E3,E2,E1,G], n = 2 in E5
;       (* n 2) E4:[E3,E2,E1,G], n = 3 in E4
;     (* n 6) E3:[E2,E1,G], n = 4 in E3
;   (* n 24) E2:[E1,G], n = 5 in E2
; (* n 120) E1:[G], n = 6 in E1
; 720

(end-script)
