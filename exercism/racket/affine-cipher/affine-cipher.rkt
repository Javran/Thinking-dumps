#lang racket

(require racket/contract)

(provide (contract-out
          [encode (string?
                   exact-nonnegative-integer?
                   exact-nonnegative-integer? . -> . string?)]
          [decode (string?
                   exact-nonnegative-integer?
                   exact-nonnegative-integer? . -> . string?)]))

(define (egcd-result? x)
  (match x
    [`(,a ,b . ,c) (and (integer? a) (integer? b) (integer? c))]
    [else #f]))

(define/contract (extended-gcd a b)
  (exact-nonnegative-integer? exact-nonnegative-integer? . -> . egcd-result?)
  ;; (extended-gcd a b) produces (u x . y) such that u = (gcd a b) and x*a + y*b = 1
  (let loop ([rst-0 (cons a '(1 . 0))]
             [rst-1 (cons b '(0 . 1))])
    (match
     (cons rst-0 rst-1)
     [`((,r0 ,s0 . ,t0) . (,r1 ,s1 . ,t1))
      (if (zero? r1)
          rst-0
          (let ([q (quotient r0 r1)]
                [r2 (remainder r0 r1)])
            (let ([s2 (- s0 (* q s1))]
                  [t2 (- t0 (* q t1))])
              (loop
               rst-1
               (cons r2 (cons s2 t2))))))])))

(define (char-destruct
         when-lower
         when-digit
         when-space
         otherwise)
  ;; analyse a char and dispatch into different cases.
  (lambda (ch)
    (cond
      [(char=? ch #\space) when-space]
      [(char-lower-case? ch)
       (when-lower ch)]
      [(char-numeric? ch)
       (when-digit ch)]
      [else (otherwise ch)])))

(define char-int? (integer-in 0 25))

(define/contract int-over-lower-char
  ((char-int? . -> . char-int? ) . -> . (char? . -> . char?))
  ;; allow a function to operate a lowercase char as if it's an int in 0..25
  (let ([base (char->integer #\a)])
    (lambda (modify)
      (lambda (ch)
        ;; assuming ch is a lowercase char
        (let ([val (- (char->integer ch) base)])
          (integer->char (+ (modify val) base)))))))

(define/contract (group-encoded chars)
  ((listof char?) . -> . string?)
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
        (call-with-values
         (lambda () (split-at xs 5))
         (lambda (ys zs)
           (loop (pack-new-item ys) zs))))))

(define (encode msg a b)
  (define encode-char
    (char-destruct
     ;; when-lower
     (int-over-lower-char
      (lambda (val) (modulo (+ (* a val) b) 26)))
     ;; when-digit
     (lambda (ch) ch)
     ;; when-space
     #f
     ;; otherwise
     (lambda (_) #f)))
  (unless (= (car (extended-gcd a 26)) 1)
    (raise (error 'not-coprime)))
  
  (group-encoded
   (filter-map encode-char (string->list (string-downcase msg)))))

(define (decode msg a b)
  (match
      (extended-gcd a 26)
    [`(1 ,s . ,_)
     (define decode-char
       (char-destruct
        ;; when-lower
        (int-over-lower-char
         (lambda (val) (modulo (* s (- val b)) 26)))
        ;; when-digit
        (lambda (ch) ch)
        ;; when-space
        #f
        ;; otherwise
        (lambda (_) #f)))
     (list->string (filter-map decode-char (string->list msg)))]
    [_ (raise (error 'not-coprime))]))

