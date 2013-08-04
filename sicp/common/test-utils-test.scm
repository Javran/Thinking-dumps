(load "./utils.scm")
(load "./test-utils.scm")

; test do-test-ex
(let ((on-correct 
         (lambda (tc)
           (cons 1 0)))
      (on-wrong
         (lambda (tc res)
           (cons 0 1)))
      ; (list (cons 1 #t) (cons 2 #t) ... (cons 1000 #t))
      (testcases (map (lambda (num) (cons num #t))
                      (list-in-range 1 1000)))
      (proc (lambda (num)
              (= 0 (modulo num 4)))))
  (let ((test-results (do-test-ex proc testcases eq? on-correct on-wrong)))
    (assert (= (apply + (map car test-results)) 250))
    (assert (= (apply + (map cdr test-results)) 750))))

(do-test
  +
  (list
    ; (+ 1 2 3) => 6
    (mat 1 2 3 
         6)
    ; (+ 4 5 6) => 15
    (mat 4 5 6
         15)))

(end-script)
