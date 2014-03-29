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

(define (factorial-itr n) (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))

; for simplicity: p=product, c=counter, m=max-count
(out (factorial-itr 6))
; (factorial-itr 6) : G
; (fact-iter 1 1 n) : E1:[G], n = 6 in E1
; (if (> c m) p (fact-iter (* c p) (+ c 1) m)) : E2:[E1,G], p = 1, c = 1, m = 6 in E2
; (fact-iter (* c p) (+ c 1) m) : E2:[E1,G]
; (if (> c m) p (fact-iter (* c p) (+ c 1) m)) : E3:[E2,E1,G], p = 1, c = 2, m = 6 in E3
; (fact-iter (* c p) (+ c 1) m) : E3:[E2,E1,G]
; (if (> c m) p (fact-iter (* c p) (+ c 1) m)) : E4:[E3,E2,E1,G], p = 2, c = 3, m = 6 in E4
; (fact-iter (* c p) (+ c 1) m) : E4:[E3,E2,E1,G]
; (if (> c m) p (fact-iter (* c p) (+ c 1) m)) : E5:[E4,E3,E2,E1,G], p = 6, c = 4, m = 6 in E5
; (fact-iter (* c p) (+ c 1) m) : E5:[E4,E3,E2,E1,G]
; (if (> c m) p (fact-iter (* c p) (+ c 1) m)) : E6:[E5,E4,E3,E2,E1,G], p = 24, c = 5, m = 6 in E6
; (fact-iter (* c p) (+ c 1) m) : E6:[E5,E4,E3,E2,E1,G]
; (if (> c m) p (fact-iter (* c p) (+ c 1) m)) : E7:[E6,E5,E4,E3,E2,E1,G], p = 120, c = 6, m = 6 in E7
; (fact-iter (* c p) (+ c 1) m) : E7:[E6,E5,E4,E3,E2,E1,G]
; (if (> c m) p _) : E8:[E7,E6,E5,E4,E3,E2,E1,G], p = 720, c = 7, m = 6 in E8
; 720

(end-script)
