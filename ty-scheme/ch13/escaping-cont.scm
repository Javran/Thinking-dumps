(load "../common/utils.scm")

(define list-product-1
  (lambda (s)
    (let recur ((s s))
      (if (null? s) 1
        (* (car s) (recur (cdr s)))))))

(out (list-product-1 '(1 2 3 4 5 6 7 8 9 10)))
; 3628800 

(out (list-product-1 '(1 2 3 4 0 6 7 8 9 10)))
; 0

; actually we don't have to go ahead if the product comes to 0 at any moment

(define list-product-2
  (lambda (s)
    (call/cc
      (lambda (exit)
        (let recur ((s s))
          (if (null? s) 1
            (if (= (car s) 0)
              (exit 0)
              (* (car s) (recur (cdr s))))))))))

(out (list-product-2 '(1 2 3 4 5 6 7 8 9 10)))
; 3628800 

(out (list-product-2 '(1 2 3 4 0 6 7 8 9 10)))
; 0

; implement "same-fringe?"

; first we need to flatten the tree
(define flatten
  (lambda (tree)
    (cond
      ; empty trees are flattened to empty lists
      ((null? tree)
          '())
      ; first element is a nested list?
      ((pair? (car tree))
          (append (flatten (car tree))
                  (flatten (cdr tree))))
      (else
        ; keep the first element, and append flatten results to it
        (cons (car tree)
              (flatten (cdr tree)))))))

(apply out
  (map
   flatten
   '(
     (1 (2 3))
     ((1 2) 3)
     (1 2 3)
     (1 (3 2))
     (((1 2) (3) (4 5)) 6 7 8))))

; we don't need to implement "same-fringe?" by direct flatten trees actually
; why not just implement some sequence equality check and pass two flatten tree (seq) in?
(define same-list?
  (lambda (list1 list2)
    (cond
      ; are they all empty lists?
      ((and (null? list1) (null? list2)) #t)
      ; ok, they are not all empty lists,
      ; is some of them empty?
      ((or (null? list1) (null? list2)) #f)
      ; are the first elements of them equal?
      ((eqv? (car list1) (car list2))
       (same-list? (cdr list1) (cdr list2)))
      (else #f))))

(define same-fringe?
  (lambda (tree1 tree2)
    (same-list? (flatten tree1) (flatten tree2))))

(out (same-fringe? '(1 (2 3)) '((1 2) 3)))
; #t

(out (same-fringe? '(1 2 3) '(1 (3 2))))
; #f

(out (same-fringe? '(1 (2 3 (4 (5)) 6) 7 8 ((9))) '((1 2 3) (4 5 6) (7 8 9))))
; #t

; a generator(iterator) here to help us traverse through tree
(define tree->generator
  (lambda (tree)
    ; place holder for caller
    (let ((caller '*))
      (letrec
        ((generate-leaves
           (lambda ()
             (let loop ((tree tree))
               (cond
                 ; tree is empty
                 ((null? tree) 'skip)
                 ; traverse through first sub-tree
                 ; and traverse through rest elements
                 ((pair? tree)
                  (loop (car tree))
                  (loop (cdr tree)))
                 ; we've reached a leaf
                 (else
                   ; when 'generate-leaves' is called, will resume from here
                   ; and the else statement will be "(else 'resume)"
                   (call/cc
                     (lambda (rest-of-tree)
                       (set! generate-leaves
                         (lambda ()
                           (rest-of-tree 'resume)))
                       ; jump to #1, and the function at #1 will be "(lambda () tree)"
                       ; i.e. returns the current leaf
                       (caller tree))))))
             ; last element passed
             (caller '()))))
        (lambda ()
          ; #1
          ; if caller is called, resume from this point with the argument
          ; the argument is always a tree
          (call/cc
            (lambda (k)
              (set! caller k)
              (generate-leaves))))))))

(define same-fringe?2
  (lambda (tree1 tree2)
    (let ((gen1 (tree->generator tree1))
          (gen2 (tree->generator tree2)))
      (let loop ()
        (let ((leaf1 (gen1))
              (leaf2 (gen2)))
          (if (eqv? leaf1 leaf2)
            (if (null? leaf1) #t (loop))
            #f))))))

(out (same-fringe?2 '(1 (2 3)) '((1 2) 3)))
; #t

(out (same-fringe?2 '(1 2 3) '(1 (3 2))))
; #f

(out (same-fringe?2 '(1 (2 3 (4 (5)) 6) 7 8 ((9))) '((1 2 3) (4 5 6) (7 8 9))))
; #t
