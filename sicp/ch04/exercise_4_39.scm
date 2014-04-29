(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

(define prog-1
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
       ;; initialize each person with all possible choices
       (let ((baker (amb 1 2 3 4 5))
             (cooper (amb 1 2 3 4 5))
             (fletcher (amb 1 2 3 4 5))
             (miller (amb 1 2 3 4 5))
             (smith (amb 1 2 3 4 5)))

         ;; puttings constraints

         ;; the floors should be distinct
         (require
          (distinct? (list baker cooper fletcher miller smith)))

         ;; Baker does not live on the top floor
         (require (not (= baker 5)))
         ;; Cooper does not live on the bottom floor
         (require (not (= cooper 1)))
         ;; Fletcher does not live on either the top or the bottom floor
         (require (not (= fletcher 5)))
         (require (not (= fletcher 1)))
         ;; Miller lives on a higher floor than does Cooper
         (require (> miller cooper))
         ;; Smith does not live on a floor adjacent to Fletcher's
         (require (not (= (abs (- smith fletcher)) 1)))
         ;; Fletcher does not live on a floor adjacent to Cooper's
         (require (not (= (abs (- fletcher cooper)) 1)))

         ;; return the possible resultss
         (list (list 'baker baker)
               (list 'cooper cooper)
               (list 'fletcher fletcher)
               (list 'miller miller)
               (list 'smith smith))))
     (multiple-dwelling)))

(define prog-2
  `(begin
     (define (distinct? items)
       (cond ((null? items) #t)
             ((null? (cdr items)) #t)
             ((member (car items) (cdr items)) #f)
             (else (distinct? (cdr items)))))

     (define (multiple-dwelling)
       ;; same as the previous one,
       ;; here we just rearranging some constraints
       ;; to make it a little bit faster
       ;; just like what I've done in the Haskell counterpart
       (let ((baker (amb 1 2 3 4 5))
             (cooper (amb 1 2 3 4 5))
             (fletcher (amb 1 2 3 4 5))
             (miller (amb 1 2 3 4 5))
             (smith (amb 1 2 3 4 5)))

         ;; puttings constraints

         ;; the floors should be distinct
         (require
          (distinct? (list baker cooper fletcher miller smith)))

         ;; Smith does not live on a floor adjacent to Fletcher's
         (require (not (= (abs (- smith fletcher)) 1)))
         ;; Fletcher does not live on a floor adjacent to Cooper's
         (require (not (= (abs (- fletcher cooper)) 1)))
         ;; Fletcher does not live on either the top or the bottom floor
         (require (not (= fletcher 5)))
         (require (not (= fletcher 1)))
         ;; Miller lives on a higher floor than does Cooper
         (require (> miller cooper))
         ;; Baker does not live on the top floor
         (require (not (= baker 5)))
         ;; Cooper does not live on the bottom floor
         (require (not (= cooper 1)))
         ;; return the possible resultss
         (list (list 'baker baker)
               (list 'cooper cooper)
               (list 'fletcher fletcher)
               (list 'miller miller)
               (list 'smith smith))))
     (multiple-dwelling)))

(define result1 (time-test amb-eval-all prog-1 (amb-init-env)))
(define result2 (time-test amb-eval-all prog-2 (amb-init-env)))

;; the second one is slightly faster
(out "result1:" result1
     "result2:" result2)

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
