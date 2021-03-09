#lang racket

(require srfi/1)
(provide encode decode)

(define top-mask #b10000000)
(define payload-mask #b01111111)

(define (encode-one n)
  (if (zero? n)
      '(0)
      (let loop ([mask 0]
                 [x n]
                 [xs '()])
        (if (zero? x)
            xs
            (let ([cur-byte (bitwise-ior
                             mask
                             (bitwise-and x payload-mask))])
              (loop
               top-mask
               (arithmetic-shift x -7)
               (cons cur-byte xs)))))))

(define (encode . nums)
  (append-map encode-one nums))

(define (decode-one nums)
  (call-with-values
   (lambda () (span (compose1 not zero? (curry bitwise-and top-mask)) nums))
   (lambda (xs ys)
     (cond
       [(> (length xs) 4)
        ;; at most 32 bits are encoded into 5 elements,
        ;; so (length xs) can be 4 at most.
        (raise (error 'sequence-too-long))]
       [(and (= (length xs) 4) (> (car xs) #b10001111))
        ;; in theory 5 bytes can encode a 35 bit integer,
        ;; here we want to make sure that top 3 bit is not used.
        (raise (error 'too-many-bits))]
       [else 
        (match ys
          ['() (raise (error 'invalid))]
          [(cons z zs)
           (cons
            (foldl
             (lambda (d acc) (bitwise-ior d (arithmetic-shift acc 7)))
             0
             (append (map (curry bitwise-and payload-mask) xs) (list z)))
            zs)])]))))

(define (decode . nums)
  (let loop ([acc '()]
             [one-result (decode-one nums)])
    (match one-result
      [(cons r rest)
       (if (null? rest)
           (reverse (cons r acc))
           (loop (cons r acc) (decode-one rest)))])))
