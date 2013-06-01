(load "../common/utils.scm")

; need to implement a list flatten function to achieve my solution
(define (flatten ls)
  (fold-right append nil ls))

; try to work it out by myself first...
(define (my-solution-1 n)
  ; first generate i
  (let ((all-i (list-in-range 1 n)))
    ; then generate j
    (let ((all-i-j (map
                     (lambda (i)
                       ; for each i in list, we map it to a list of pair (i,j)
                       ;    this is done by first make a list: all-j
                       (let ((all-j (list-in-range 1 (- i 1))))
                         ; then construct the list by adding i
                         (map (lambda (x) (cons i x)) all-j)))
                     all-i)))
      ; now we need to flatten the list
      (let ((flat-pairs (flatten all-i-j)))
        ; pick up valid pairs and make the tuple
        (filter identity 
                (map (lambda (pair)
                  (let ((a (car pair))
                        (b (cdr pair)))
                    (if (prime? (+ a b))
                      (list a b (+ a b))
                      #f)))
                flat-pairs))))))

(out (my-solution-1 6))
; ((2 1 3) (3 2 5) ... (6 5 11))

(end-script)
