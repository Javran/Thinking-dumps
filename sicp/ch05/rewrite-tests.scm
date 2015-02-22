(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "rewrite.scm")

(define (bag-equal? xs ys)
  (define (bag-subset? xs ys)
    ;; xs is a subset of ys
    ;; if all elements in xs
    ;; can also be found in ys
    (fold-left
     (lambda (a b) (and a b))
     #t
     (map (lambda (x)
            (member x ys))
          xs)))
  (and (bag-subset? xs ys)
       (bag-subset? ys xs)))

(define (assoc-equal? xs ys)
  (if (and xs ys)
      ;; both xs and ys aren't false
      (bag-equal? xs ys)
      (eq? xs ys)))

(do-test
 template-variable?
 (list
  (mat 'abc #f)
  (mat '$a #t)
  (mat '() #f)))

(do-test
 pattern-match
 (list
  (mat '($a $b ($a $c))
       '(10 20 (10 30))
       '()
       '(($a 10)
         ($b 20)
         ($c 30)))
  (mat '($a $b $a)
       '(10 20 20)
       '()
       #f)
  (mat '($a (a b c $d) $e)
       '(ax (a b c dd) eee)
       '(($a ax))
       '(($a ax)
         ($d dd)
         ($e eee))))
 assoc-equal?)

(do-test
 template-apply
 (list
  (mat '($a a b c)
       '(($a 1))
       '(1 a b c))
  (mat '($a $b $a)
       '(($a (a b c d))
         ($b (e . f)))
       '((a b c d) (e . f) (a b c d)))))

(do-test
 (lambda (data)
   (let ((rules
          '( (cat dog)
             (open close)
             ( (swap $a $b) ($b $a) ))))
     (try-rewrite-once rules data)))
 (list
  (mat 'open 'close)
  (mat 'cat 'dog)
  (mat 'norule #f)
  (mat '(swap (a b c d) (e f g))
       '((e f g) (a b c d)))))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
