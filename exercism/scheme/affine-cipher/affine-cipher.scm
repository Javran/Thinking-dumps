(import (rnrs))

(use-modules ((srfi srfi-1) #:select (filter-map split-at)))
(use-modules (ice-9 match))

(define (extended-gcd a b)
  ;; note: this impl assumes non-negative a and b.
  (let loop ([rst-0 (cons a '(1 . 0))]
             [rst-1 (cons b '(0 . 1))])
    (match
     (cons rst-0 rst-1)
     [((r0 s0 . t0) . (r1 s1 . t1))
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

(define int-over-lower-char
  ;; allow a function to operate a lowercase char as if it's an int in 0..25
  (let ([base (char->integer #\a)])
    (lambda (modify)
      (lambda (ch)
        ;; assuming ch is a lowercase char
        (let ([val (- (char->integer ch) base)])
          (integer->char (+ (modify val) base)))))))

(define (group-encoded chars)
  ;; takes a list of chars and turns it into a string of space-separated
  ;; groups, in which each group consists of 5 characters.
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

(define (encode key text)
  (match
   key
   [(a . b)
    (define encode-char
      (destruct-char
       ;; when-lower
       (int-over-lower-char
        (lambda (val) (modulo (+ (* a val) b) 26)))
       ;; when-digit
       (lambda (ch) ch)
       ;; when-space
       #f
       ;; otherwise
       (lambda (_) #f)))
    (group-encoded
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
         (int-over-lower-char
          (lambda (val) (modulo (* s (- val b)) 26)))
         ;; when-digit
         (lambda (ch) ch)
         ;; when-space
         #f
         ;; otherwise
         (lambda (_) #f)))
      (list->string (filter-map decode-char (string->list text)))]
     [_ (raise 'not-a-coprime)])]))
