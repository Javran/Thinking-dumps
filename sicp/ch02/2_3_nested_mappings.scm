(load "../common/utils.scm")

; need to implement a list flatten function to achieve my solution
(define (flatten ls)
  (fold-right append nil ls))

; try to work it out by myself first...
;   wow poor readability boy...
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

(define (solution-part-1 n)
  (fold-right
    append
    nil
    (map (lambda (i)
           (map (lambda (j) (list i j))
                (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 n))))

(out (solution-part-1 6))
; all i-j pairs that 1 <= j < i <= n

(define (flatmap f seq)
  (fold-right append nil (map f seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                 (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n)))))

(out (prime-sum-pairs 6))
(out (my-solution-1 6))
; ((2 1 3) (3 2 5) ... (6 5 11))

(define (permutations s)
  (if (null? s)
    (list nil)
    (flatmap (lambda (x)
               ; for each element x in list
               (map (lambda (p) (cons x p))
                    (permutations (remove-from-list x s))))
             s)))

(define (remove-from-list item seq)
  (filter (lambda (x) (not (= x item)))
          seq))

(out (permutations (list 1 2 3)))

(end-script)
