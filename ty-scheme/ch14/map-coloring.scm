(load "../common/utils.scm")
(load "../common/amb.scm")

; provide all all alternative colors
(define choose-color
  (lambda ()
    (amb 'red
         'yellow
         'blue
         'white)))

(out (bag-of (choose-color)))
; print a list of all colors

(define color-europe
  (lambda ()
    (let* ((p (choose-color)) ; Portugal
           (e (choose-color)) ; Spain
           (f (choose-color)) ; France
           (b (choose-color)) ; Belgium
           (h (choose-color)) ; Holland
           (g (choose-color)) ; Germany
           (l (choose-color)) ; Luxemb
           (s (choose-color)) ; Switz
           (a (choose-color)) ; Austria
          )

(out (bag-of (color-europe)))
