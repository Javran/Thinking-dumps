#lang racket

(require srfi/1)
(provide encode decode)

(define top-mask #b10000000)
(define payload-mask #b01111111)

(define (encode-one n)
  (if (zero? n)
      '(0)
      (let loop ([last-bit #t]
                 [x n]
                 [xs '()])
        (if (zero? x)
            xs
            (let ([cur-byte (bitwise-ior (if last-bit 0 top-mask)
                                         (bitwise-and x payload-mask))])
              (loop #f (arithmetic-shift x -7) (cons cur-byte xs)))))))
              
(define (encode . nums)
  (append-map encode-one nums))

(define (decode-one nums)
  (call-with-values
   (lambda () (span (lambda (x) (not (zero? (bitwise-and top-mask x)))) nums))
   (lambda (xs ys)
     (if (> (length xs) 4)
         ;; at most 32 bits are encoded into 5 elements,
         ;; so (length xs) can be 4 at most.
         (raise (error 'invalid))
         (match ys
           ['() (raise (error 'invalid))]
           [(cons z zs)
            (cons
             (foldl
              (lambda (d acc) (bitwise-ior d (arithmetic-shift acc 7)))
              0
              (append (map (lambda (x) (bitwise-and payload-mask x)) xs) (list z)))
             zs)])))))

(define (decode . nums)
  (let loop ([acc '()]
             [one-result (decode-one nums)])
    (match one-result
      [(cons r remaining)
       (if (null? remaining)
           (reverse (cons r acc))
           (loop (cons r acc) (decode-one remaining)))])))
