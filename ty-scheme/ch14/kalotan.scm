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

(newline)

(define xor
  (lambda (a b)
    (if (eqv? a b)
      #f
      #t)))

; somewhat similiar to list comprehension with guards
(define solve-kalotan-puzzle
  (lambda ()
    (let ((parent1 (amb 'm 'f))
          (parent2 (amb 'm 'f))
          (kibi (amb 'm 'f))
          (kibi-self-desc (amb 'm 'f))
          (kibi-lied? (amb #t #f)))

      (assert
        (distinct? (list parent1 parent2)))
      (assert
        ; if kibi is male, he'll never tell lie
        (if (eqv? kibi 'm)
          (not kibi-lied?)))
      (assert
        ; if kibi lied, self-desc would not correspond with truth
        (if kibi-lied?
          (xor
            ; either case #1 or case #2
            ; case #1
            (and (eqv? kibi-self-desc 'm)
                 (eqv? kibi 'f))
            ; case #2
            (and (eqv? kibi-self-desc 'f)
                 (eqv? kibi 'm)))))
      (assert
        (if (not kibi-lied?)
          ; elsewise, self-desc would correspond with truth
          (xor
            ; either case #1 or case #2
            ; case #1
            (and (eqv? kibi-self-desc 'm)
                 (eqv? kibi 'm))
            ; case #2
            (and (eqv? kibi-self-desc 'f)
                 (eqv? kibi 'f)))))
      (assert
        ; male would not lie
        (if (eqv? parent1 'm)
          (and
            ; and kibi's desc was "I am a boy"
            (eqv? kibi-self-desc 'm)

            ; parent2 said:
            (xor
              ; 1: kibi is a girl
              ; 2: kibi lied
              (and (eqv? kibi 'f)         ; 1
                   (eqv? kibi-lied? #f))  ; 2

              (and (eqv? kibi 'm)         ; 1 
                   (eqv? kibi-lied? #t)))); 2
          ))

      (assert
        ; parent1 -> femail
        (if (eqv? parent1 'f)
          ; male would not lie
          (and
            (eqv? kibi 'f)
            (eqv? kibi-lied? #t))))
      (list parent1 parent2 kibi))))

(out (bag-of (solve-kalotan-puzzle)))
; (f m f)

; FYI: what if no limitation is set?
(define no-limit-test
  (lambda ()
    (let ((parent1 (amb 'm 'f))
          (parent2 (amb 'm 'f))
          (kibi (amb 'm 'f))
          (kibi-self-desc (amb 'm 'f))
          (kibi-lied? (amb #t #f)))

      (list parent1 parent2 kibi))))

(out (bag-of (no-limit-test)))
; all combinations are shown

; let's try something easier!
(define number-between
  (lambda (lo hi)
    (let loop ((i lo))
      (if (> i hi) (amb)
        (amb i (loop (+ i 1)))))))
; for 3 numbers, a, b, c, s.t. 1 <= a <= b <= c <= 10 & a^2 + b^2 = c^2

(define right-triangle
  (lambda ()
    (let* ((a (number-between 1 10))
           (b (number-between a 10))
           (c (number-between b 10)))
      (assert (= 
                (+ (* a a) (* b b))
                (* c c)))
      (list a b c))))

(out (bag-of (right-triangle)))
; well, we have (3 4 5) and (6 8 10) :)
