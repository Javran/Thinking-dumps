(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

(define prog
  `(begin
     (define (length xs)
       (if (null? xs)
           0
           (+ 1 (length (cdr xs)))))

     (define (list-diff xs ys)
       (if (null? ys)
           xs
           (delete (car ys)
                   (list-diff xs (cdr ys)))))

     (define (reverse xs)
       (if (null? xs)
           '()
           (append (reverse (cdr xs))
                   (list (car xs)))))

     (define (count-down xs c)
       (if (null? xs)
           '()
           (cons (cons (car xs) c)
                 (count-down (cdr xs) (- c 1)))))

     (define (fold-left go init xs)
       (if (null? xs)
           init
           (fold-left go (go init (car xs)) (cdr xs))))

     (define (eight-queens current-board)
       (let ((len (length current-board)))
         (if (= 8 len)
             current-board
             (let ((all1 (list-diff
                           '(0 1 2 3 4 5 6 7)
                           current-board))
                   (limit-pairs (count-down (reverse current-board) len)))
               (let ((all2 (fold-left
                            (lambda (candicates i)
                              (let ((pos (car i))
                                    (offset (cdr i)))
                                (delete (- pos offset)
                                        (delete (+ pos offset)
                                                candicates))))
                            all1
                            limit-pairs)))
                 (let ((next (an-element-of all2)))
                   (eight-queens (cons next current-board))))))))
     (eight-queens '())))

;; one solution for example, is:
;; (3 1 6 2 5 7 4 0)
(define (print-board board)
  ;; board visualization
  (out "  01234567")
  (for-each (lambda (p)
              (let ((c (car p))
                    (pos (cdr p)))
                (format #t "~A ~A~%"
                        c
                        (string-pad-left "x" pos))))
            (map cons (list-in-range 0 7)
                 (reverse board)))
  (newline))

(define all-solutions
  (amb-eval-all prog (amb-init-env)))

(for-each print-board all-solutions)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
