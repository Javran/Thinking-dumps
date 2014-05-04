(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

;; just Haskell version rewritten in my amb-eval.
;; please find "./exercise_4_43.scm" for comments

(define prog
  `(begin
     (define (map f xs)
       (if (null? xs)
           '()
           (cons (f (car xs))
                 (map f (cdr xs)))))

     (define (lookup key dict)
       (if (null? dict)
           '()
           (let ((pair (car dict)))
             (if (eq? (car pair) key)
                 pair
                 (lookup key (cdr dict))))))

     (define first-names
       '(maryann
         lorna
         melissa
         rosalind
         gabrielle))

     (define last-names
       '(moore
         downing
         hall
         hood
         parker))

     (let ((y-hood 'gabrielle)
           (y-moore 'lorna)
           (y-hall 'rosalind)
           (y-downing 'melissa)
           (l-melissa 'hood)
           (l-maryann 'moore))
       (let ((l-lorna (an-element-of last-names))
             (l-rosalind (an-element-of last-names))
             (l-gabrielle (an-element-of last-names))
             (y-parker (an-element-of first-names)))
         (require (not (eq? l-lorna 'moore)))
         (require (not (eq? l-rosalind 'hall)))
         (require (not (eq? l-gabrielle 'hood)))

         (let ((yacht-owns
                (list (cons 'moore y-moore)
                      (cons 'downing y-downing)
                      (cons 'hall y-hall)
                      (cons 'hood y-hood)
                      (cons 'parker y-parker)))
               (daughter-is
                (list (cons l-maryann 'maryann)
                      (cons l-lorna 'lorna)
                      (cons l-melissa 'melissa)
                      (cons l-rosalind 'rosalind)
                      (cons l-gabrielle 'gabrielle))))
           (define full-name daughter-is)

           (require (distinct?
                     (map cdr yacht-owns)))
           (require (distinct?
                     (map car daughter-is)))

           (require
            (eq?
             (cdr (lookup l-gabrielle yacht-owns))
             (cdr (lookup 'parker daughter-is))))
           full-name)))))

(out (amb-eval-all prog (amb-init-env)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
