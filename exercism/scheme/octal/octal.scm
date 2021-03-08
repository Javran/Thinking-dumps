(import (rnrs))

(define (to-decimal s)
  (define ch-base (char->integer #\0))
  (call/cc
   (lambda (k)
     (let loop ([xs (string->list s)]
                [acc 0])
       (cond
        [(null? xs) acc]
        [(char<=? #\0 (car xs) #\7)
         (loop (cdr xs)
               (+ (* acc 8)
                  (- (char->integer (car xs)) ch-base)))]
        [else (k 0)])))))
