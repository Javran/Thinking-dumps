(load "../common/utils.scm")

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((equal? given-key (key (car set-of-records)))
          (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

; extra task: implement 'lookup' for unordered list

(define make-entry cons)
(define key car)
(define value cdr)

(let ((records '((1 . one)
                 (2 . two)
                 (3 . three)
                 (4 . four)
                 (5 . five)
                 (6 . six))))
  ; lookup elements and display the result
  (for-each
    (lambda (entry)
      (if entry
        (out (value entry))
        (out "<No record>")))
    (map
      (lambda (k) (lookup k records))
      (list-in-range 1 10))))

(end-script)
