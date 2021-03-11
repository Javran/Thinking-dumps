#lang racket

;; Converts integers to English-language descriptions

(define max-bound 999999999999999)
(define say-input? (integer-in 0 max-bound))
(define int-chunk? (integer-in 0 999))

(provide
 (contract-out
  [step1 (-> (integer-in 0 99) string?)]
  ;; Convert a non-negative, 2-digit number to an English string

  [step2 (-> say-input? (listof int-chunk?))]
  ;; Divide a large positive number into a list of 3-digit (or smaller) chunks

  [step3 (-> say-input? (listof (cons/c int-chunk? symbol?)))]
  ;; Break a number into chunks and insert scales between the chunks

  [step4 (-> (integer-in (- max-bound) max-bound) string?)]
  ;; Convert a number to an English-language string
  ))

(define say-0-to-19
  (append
   (string-split
    "zero one two three four five six seven eight nine")
   (string-split
    "ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen ninteen")))

(define say-tys-20-to-90
  (string-split
   "twenty thirty forty fifty sixty seventy eighty ninety"))

(define (say-0-to-99 n)
  (if (<= n 19)
      (list-ref say-0-to-19 n)
      (let-values ([(q r) (quotient/remainder n 10)])
        (let ([xs (list-ref say-tys-20-to-90 (- q 2))])
          (if (zero? r)
              xs
              (~a xs #\- (list-ref say-0-to-19 r)))))))

(define (say-int-chunk n)
  (let-values ([(q r) (quotient/remainder n 100)])
    (let ([lo (say-0-to-99 r)])
      (if (zero? q)
          lo
          (let ([hi (~a (list-ref say-0-to-19 q) #\space "hundred")])
            (if (zero? r)
                hi
                (~a hi #\space lo)))))))

(define (integer->int-chunks n)
  (let loop ([val n]
             [chunks '()])
    (if (zero? val)
        chunks
        (let-values ([(q r) (quotient/remainder val 1000)])
          (loop q (cons r chunks))))))

(define scale-words
  '(END thousand million billion trillion))

(define scale-pair->string
  (match-lambda
    [`(0 . ,_) #f]
    [(cons n w)
     (let ([xs  (say-int-chunk n)])
       (if (eq? w 'END)
           xs
           (~a xs #\space (symbol->string w))))]))

(define (scale-paired-chunks n)
  (let* ([chunks (integer->int-chunks n)]
         [len (length chunks)]
         [wds (take scale-words len)])
    (map cons chunks (reverse wds))))

(define (say n)
  (cond
    [(zero? n) "zero"]
    [(< n 0) (~a "negative " (say (- n)))]
    [else (string-join
           (filter-map
            scale-pair->string
            (scale-paired-chunks n)))]))

;; those are horrible names, not gonna refer to any of them anywhere in impl body.
(define step1 say-0-to-99)
(define step2 integer->int-chunks)
(define step3 scale-paired-chunks)
(define step4 say)
