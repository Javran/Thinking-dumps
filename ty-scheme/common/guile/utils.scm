; this file is used for Guile, another scheme implementation

(define out
  (lambda items
    (map (lambda (x)
           (display x)
           (newline)) items)))
