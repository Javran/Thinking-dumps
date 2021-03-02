(import (rnrs))

(use-modules (ice-9 match))

(define (balanced? s)
  (let verify ([i 0]
               [r-parens '()])
    (if (= i (string-length s))
        (null? r-parens)
        (let ([ch (string-ref s i)])
          (match
           ch
           [#\( (verify (+ i 1) (cons #\) r-parens))]
           [#\[ (verify (+ i 1) (cons #\] r-parens))]
           [#\{ (verify (+ i 1) (cons #\} r-parens))]
           [(or #\) #\] #\})
            (and (not (null? r-parens))
                 (char=? (car r-parens) ch)
                 (verify (+ i 1) (cdr r-parens)))]
           [_ (verify (+ i 1) r-parens)])))))
