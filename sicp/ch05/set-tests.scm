(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./set.scm")

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
 (lambda (actual expect)
   (or (and actual expect)
       (and (not actual) (not expect)))))

(end-script)
