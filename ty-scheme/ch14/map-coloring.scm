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
    (let* (
           
           ; the search space: pick up one color for each country
           (p (choose-color)) ; Portugal
           (e (choose-color)) ; Spain
           (f (choose-color)) ; France
           (b (choose-color)) ; Belgium
           (h (choose-color)) ; Holland
           (g (choose-color)) ; Germany
           (l (choose-color)) ; Luxemb
           (i (choose-color)) ; Italy
           (s (choose-color)) ; Switz
           (a (choose-color)) ; Austria

           ; construct the adjacency list for each country
           ; list struct: (list 
           ;                  <country name>
           ;                  <country color> 
           ;                  <list of neighbors' color>)
           (portugal
             (list 'portugal p
                   (list 
                     ; Spain
                     e)))
           (spain
             (list 'spain e
                   (list
                     ; France
                     ; Portugal
                     f p)))
           (france
             (list 'france f
                   (list
                     ; Spain
                     ; Italy
                     ; Switz
                     ; Belgium
                     ; Germany
                     ; Luxemb
                     e i s b g l)))
           (belgium
             (list 'belgium b
                   (list
                     ; France
                     ; Holland
                     ; Luxemb
                     ; Germany
                     f h l g)))
           (holland
             (list 'holland h
                   (list
                     ; Belgium
                     ; Germany
                     b g)))
           (germany
             (list 'germany g
                   (list
                     ; France
                     ; Austria
                     ; Switz
                     ; Holland
                     ; Belgium
                     ; Luxemb
                     f a s h b l)))
           (luxemb
             (list 'luxemb l
                   (list
                     ; France
                     ; Belgium
                     ; Germany
                     f b g)))
           (italy
             (list 'italy i
                   (list
                     ; France
                     ; Austria
                     ; Switz
                     f a s)))
           (switz
             (list 'switz s
                   (list
                     ; France
                     ; Italy
                     ; Austria
                     ; Germany
                     f i a g)))
           (austria
             (list 'austria a
                   (list
                     ; Italy
                     ; Switz
                     ; Germany
                     i s g)))

           (countries
             (list
               portugal
               spain
               france
               belgium
               holland
               germany
               luxemb
               italy
               switz
               austria))
          )
      (for-each
        (lambda (c)
          (assert
            ; the color of a country should not be the color of
            ; any of its neighbors
            (not (memq (cadr c)
                       (caddr c)))))
        countries)

      ; only the first two elements are kept for prettier printing
      (map
        (lambda (l)
          (list (car l) (cadr l)))
        countries))))

; (out (bag-of color-europe))
; it'll take some time to get all solutions

(out (color-europe))
