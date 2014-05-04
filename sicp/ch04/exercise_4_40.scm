(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

(define prog
  `(begin
     ;; test if each of the elements in the list
     ;; is distinct
     (define (distinct? items)
       (cond ((null? items) #t)
             ((null? (cdr items)) #t)
             ((member (car items) (cdr items)) #f)
             (else (distinct? (cdr items)))))

     ;; the problem itself
     (define (multiple-dwelling)

       (let ((choices '(1 2 3 4 5)))
         (let ((fletcher (an-element-of choices)))
           ;; Fletcher does not live on either the top or the bottom floor
           (require (not (= fletcher 5)))
           (require (not (= fletcher 1)))

           (let ((smith (an-element-of choices)))
             ;; Smith does not live on a floor adjacent to Fletcher's
             (require (not (= (abs (- smith fletcher)) 1)))

             (let ((cooper (an-element-of choices)))
               ;; Cooper does not live on the bottom floor
               (require (not (= cooper 1)))

               ;; Fletcher does not live on a floor adjacent to Cooper's
               (require (not (= (abs (- fletcher cooper)) 1)))

               (let ((miller (an-element-of choices)))
                 ;; Miller lives on a higher floor than does Cooper
                 (require (> miller cooper))

                 (let ((baker (an-element-of choices)))
                   ;; Baker does not live on the top floor
                   (require (not (= baker 5)))

                   ;; the floors should be distinct
                   (require
                    (distinct? (list baker cooper fletcher miller smith)))

                   ;; return the possible resultss
                   (list (list 'baker baker)
                         (list 'cooper cooper)
                         (list 'fletcher fletcher)
                         (list 'miller miller)
                         (list 'smith smith)))))))))
     (multiple-dwelling)))

(define result (time-test amb-eval-all prog (amb-init-env)))

;; please compare this result with previous exercises
;; the elapsed time is only around 263, much faster than any of previous results
(out result)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
