(import (rnrs))

(use-modules ((srfi srfi-1) #:select (split-at)))
(use-modules (ice-9 match))

(define (extended-gcd a b)
  ;; note: this impl assumes non-negative a and b.
  (let loop ([rst-0 (cons a '(1 . 0))]
             [rst-1 (cons b '(0 . 1))])
    (match
     (cons rst-0 rst-1)
     [((r0 . (s0 . t0)) . (r1 . (s1 . t1)))
      (if (zero? r1)
          rst-0
          (let ([q (quotient r0 r1)]
                [r2 (remainder r0 r1)])
            (let ([s2 (- s0 (* q s1))]
                  [t2 (- t0 (* q t1))])
              (loop
               rst-1
               (cons r2 (cons s2 t2))))))])))

(define (group chars)
  (let loop ([result ""]
             [xs chars])
    (cond
     [(null? xs) result]
     [(< (length xs) 5)
      (if (string=? result "")
          (list->string xs)
          (string-append result " " (list->string xs)))]
     [else (call-with-values (lambda () (split-at xs 5))
             (lambda (ys zs)
               (loop
                (if (string=? result "")
                    (list->string ys)
                    (string-append result " " (list->string ys)))
                zs)))])))

(define (encode key text)
  ;; encode rules are annoying random, but whatever.
  ;; keep only alpha and num, convert alpha, keep num intact
  ;; and then break into groups of 5
  (match
   key
   [(a . b)
    (define (encode-char ch)
      ;; returns #f if the char cannot be encoded.
      (cond
       [(char-set-contains? char-set:digit ch) ch]
       [(char-set-contains? char-set:lower-case ch)
        (let* ([val (- (char->integer ch) (char->integer #\a))]
               [encoded (remainder (+ (* a val) b) 26)])
          (integer->char (+ (char->integer #\a) encoded)))]
       [else #f]))
    (group
     (filter (lambda (x) x)
            (map encode-char (string->list (string-downcase text)))))]))

(define (decode key text)
  ;; decode: remove spaces, convert alpha, keep num.
  'implement-me!)


(define debug #t)

(if debug
    (begin
      (display "<")(display (group (string->list "fasdfe12345"))) (display ">") (newline)
      (display
       (encode
        '(17 . 33)
        "12234 The quick brown fox jumps over the lazy dog."))
      (newline)))
