(import (rnrs))

(use-modules ((srfi srfi-1) #:select (filter-map split-at)))
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

(define (destruct-char
         when-lower
         when-digit
         when-space
         otherwise)
  (lambda (ch)
    (cond
     [(char=? ch #\space) when-space]
     [(char-set-contains? char-set:lower-case ch)
      (when-lower ch)]
     [(char-set-contains? char-set:digit ch)
      (when-digit ch)]
     [else (otherwise ch)])))

(define (encode key text)
  ;; encode rules are annoying random, but whatever.
  ;; keep only alpha and num, convert alpha, keep num intact
  ;; and then break into groups of 5
  (match
   key
   [(a . b)
    (define encode-char
      (destruct-char
       ;; when-lower
       (lambda (ch)
         (let* ([val (- (char->integer ch) (char->integer #\a))]
                [encoded (modulo (+ (* a val) b) 26)])
           (integer->char (+ (char->integer #\a) encoded))))
       ;; when-digit
       (lambda (ch) ch)
       ;; when-space
       #f
       ;; otherwise
       (lambda (_) #f)))
    (define (group chars)
      (let loop ([result ""]
             [xs chars])
        (define (pack-new-item cs)
          (if (null? cs)
              result
              ;; pack cs and generate a new result string, taking into account spaces.
              (let ([packed (list->string cs)])
                (if (string=? result "")
                    packed
                    (string-append result " " packed)))))
        (if (< (length xs) 5)
            (pack-new-item xs)
            (call-with-values (lambda () (split-at xs 5))
              (lambda (ys zs)
                (loop (pack-new-item ys) zs))))))
    (group
     (filter-map encode-char (string->list (string-downcase text))))]))

(define (decode key text)
  (match
   key
   [(a . b)
    (match
     (extended-gcd a 26)
     [(1 . (s . _))
      (define decode-char
        (destruct-char
         ;; when-lower
         (lambda (ch)
           (let* ([val (- (char->integer ch) (char->integer #\a))]
                  [decoded (modulo (* s (- val b)) 26)])
             (integer->char (+ (char->integer #\a) decoded))))
         ;; when-digit
         (lambda (ch) ch)
         ;; when-space
         #f
         ;; otherwise
         (lambda (_) #f)))

      (list->string (filter-map decode-char (string->list text)))]
     [_ (raise 'not-a-coprime)])]))

(define debug #f)

(if debug
    (begin
      (display
       (decode '(25 . 7) "odpoz ub123 odpoz ub")) (newline)
      (display
       (decode '(17 . 33) "swxtj npvyk lruol iejdc blaxk swxmh qzglf")) (newline)
      (display
       (encode
        '(17 . 33)
        "12234 The quick brown fox jumps over the lazy dog."))
      (newline)))
