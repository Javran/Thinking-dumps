(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

(define prog
  `(begin
     (define (distinct? items)
       (cond ((null? items) #t)
             ((null? (cdr items)) #t)
             ((member (car items) (cdr items)) #f)
             (else (distinct? (cdr items)))))

     (define (solve)
       (let ((places '(1 2 3 4 5)))
         (define (either-true who1 place1
                              who2 place2)
           (not (eq? (= who1 place1)
                     (= who2 place2))))
         (let ((betty (an-element-of places))
               (ethel (an-element-of places))
               (joan  (an-element-of places))
               (kitty (an-element-of places))
               (mary  (an-element-of places)))

           (require
            (distinct?
             (list betty ethel joan kitty mary)))
           ;; Betty: Kitty was second, I was only third.
           (require (either-true
                     kitty 2
                     betty 3))

           ;; Ethel: I was top. Joan was 2nd
           (require (either-true
                     ethel 1
                     joan 2))

           ;; Joan: I was third, Ethel was bottom
           (require (either-true
                     joan 3
                     ethel 5))

           ;; Kitty: I came out second, Mary was only fourth
           (require (either-true
                     kitty 2
                     mary 4))

           ;; Mary: I was fourth, top place was taken by Betty
           (require (either-true
                     mary 4
                     betty 1))

           (list (list 'betty betty)
                 (list 'ethel ethel)
                 (list 'joan joan)
                 (list 'kitty kitty)
                 (list 'mary mary)))))
     (solve)))

(out (amb-eval-all prog (amb-init-env)))
;; answer is:
;; 1 Kitty
;; 2 Joan
;; 3 Betty
;; 4 Mary
;; 5 Ethel

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
