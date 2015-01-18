;; this module contains set-related basic operations

;; sets are represented as lists, with the assumption that
;; they do not have duplicated elements

(define (set-union s1 s2)
  (cond ((null? s1) s2)
        ((member (car s1) s2) (set-union (cdr s1) s2))
        (else
         (cons (car s1)
               (set-union (cdr s1) s2)))))

(define (set-difference s1 s2)
  (cond ((null? s1) '())
        ((member (car s1) s2) (set-difference (cdr s1) s2))
        (else
         (cons (car s1)
               (list-difference (cdr s1) s2)))))

(define (set-intersection s1 s2)
  (if (null? s1)
      '()
      (let ((hd (car s1))
            (tl (cdr s1)))
        (if (member hd s2)
            (cons hd (set-intersection tl (set-delete hd s2)))
            (set-intersection tl s2)))))

(define set-empty? null?)

(define (set-insert e s)
  (if (member e s)
      s
      (cons e s)))

(define (set-delete e s)
  (if (null? s)
      '()
      (if (equal? e (car s))
          (cdr s)
          (cons (car s) (set-delete e (cdr s))))))
