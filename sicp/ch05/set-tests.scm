(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./set.scm")

;; checks if two things are equal
;; in terms of their boolean values
(define (bool-eq? a b)
  (or (and a b)
      (and (not a) (not b))))

;; test set equality and establish other things on it
(do-test
 set-equal?
 (list (mat '(1 2 3) '(1 2 3) #t)
       (mat '(1 2 3) '(3 2 1) #t)
       (mat '(1 2 3) '(4 5 6) #f)
       (mat '() '() #t)
       (mat '() '(1 2 3) #f)
       (mat '(a b) '(b c) #f)
       (mat '(a b c) '(a c) #f)
       (mat '(a c d) '(b a c g d) #f)
       (mat '(a c b d) '(d b c a) #t))
 bool-eq?)

;; set-empty?
(do-test
 set-empty?
 (list (mat '() #t)
       (mat '(a) #f)))

;; set-insert
(do-test
 set-insert
 (list (mat 'a '() '(a))
       (mat 'a '(b a c) '(a b c))
       (mat 'b '(c d) '(b c d)))
 set-equal?)

;; set-delete
(do-test
 set-delete
 (list (mat 'a '() '())
       (mat 'a '(b c d) '(b c d))
       (mat 'a '(b a c) '(b c))
       (mat 'b '(a c d e b) '(a c d e)))
 set-equal?)

;; set-union
(do-test
 set-union
 (list (mat '(a b) '(c d) '(a b c d))
       (mat '(a b) '(b a c) '(a b c))
       (mat '() '(a) '(a))
       (mat '(a c d) '(a d c) '(a c d))
       (mat '(a b c d) '(a b) '(a b c d))
       (mat '(a) '() '(a)))
 set-equal?)

;; set-difference
(do-test
 set-difference
 (list (mat '(a b) '(c d) '(a b))
       (mat '(a b) '(b a c) '())
       (mat '() '(a) '())
       (mat '(a c d) '(a d c) '())
       (mat '(a b c d) '(a b) '(c d))
       (mat '(a) '() '(a)))
 set-equal?)

;; set-intersection
(do-test
 set-intersection
 (list (mat '(a b) '(c d) '())
       (mat '(a b) '(b a c) '(a b))
       (mat '() '(a) '())
       (mat '(a c d) '(a d c) '(a c d))
       (mat '(a b c d) '(a b) '(a b))
       (mat '(a) '() '()))
 set-equal?)

;; set-subset<=?
(do-test
 set-subset<=?
 (list (mat '() '(a b) #t)
       (mat '(a) '() #f)
       (mat '() '() #t)
       (mat '(a b) '(c a b) #t)
       (mat '(c a)' (a b) #f))
 ;; boolean comparison
 (lambda (a b)
   (or (and a b)
       (and (not a) (not b)))))

(end-script)
