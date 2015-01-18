;; this module contains set-related basic operations

;; sets are represented as lists, with the assumption that
;; they do not have duplicated elements

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

(define (set-union s1 s2)
  (fold-right set-insert s1 s2))

(define (set-difference s1 s2)
  (fold-right set-delete s1 s2))

(define (set-equal? s1 s2)
  ;; if s1 - s2 = empty,
  ;; then s2 is a superset of s1.
  ;; further if s2 - s1 = empty
  ;; then s1 is a superset of s2.
  ;; => s1 and s2 are equal
  (and (set-empty? (set-difference s1 s2))
       (set-empty? (set-difference s2 s1))))

(define (set-intersection s1 s2)
  (set-difference s1
                  (set-difference s1 s2)))
