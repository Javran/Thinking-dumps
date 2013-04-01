(load "../common/utils.scm")
(load "../common/amb.scm")

(define distinct?
  (lambda (ls)
    (if (null? ls)
      #t
      (let* ((head (car ls))
             (tail (cdr ls))
             (filtered-ls (filter (lambda (x) (eqv? head x)) tail)))
        (if (null? filtered-ls)
          (distinct? tail)
          #f)))))

(out (distinct? '(1 2 3)))
; #t
(out (distinct? '(1 2 3 4 5 6)))
; #t
(out (distinct? '(1 2 3 4 5 1)))
; #f
(out (distinct? '(f m a b)))
; #t
(out (distinct? '(f m f)))
; #f

;(define solve-kalotan-puzzle
;  (lambda ()
;    (let ((parent1 (amb 'm 'f))
;          (parent2 (amb 'm 'f))
;          (kibi (amb 'm 'f))
;          (kibi-self-desc (amb 'm 'f))
;          (kibi-lied? (amb #t #f)))

