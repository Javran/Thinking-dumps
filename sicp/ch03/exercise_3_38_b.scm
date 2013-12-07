(load "../common/utils.scm")
(load "../common/test-utils.scm")

; there are 6! / (2!)^3 = 90 possibilities.
; for each person, there are two steps:
;   1. access the balance
;   2. modify and write back the balance
; if we have a sequence of 6 elements,
;   each element is in (0,1,2), and distinct element
;   occurs exactly twice, then each of these sequences is corresponding
;   to one possibility:
; if we are given 0 1 0 2 2 1, Peter -> 0, Paul -> 1, Mary -> 2
;   then we can do the following:
;   0: Peter reads balance value
;   1: Paul  reads balance value
;   0: Peter write back modified balance value
;   2: Mary  reads balance value
;   2: Mary  write back modified balance value
;   1: Paul  write back modified balance value
; that is, we put every guy to sleep and only wake up one of three
;   and allows him/her to take one step.

; step #1: generate the permutation list

(define (gen-permutation elements indices)
  ; elements: '(0 0 1 1 2 2)
  ; indices : '(0 1 2 3 4 5)
  (if (null? elements)
    (list nil)
    (concatmap
      ; 1. for each index
      (lambda (index)
        (let ((rest-indices  (delete index indices))
              (rest-elements (cdr elements)))
          (map
            ; on each permutation, we attach current choice to it
            (lambda (perm) (cons (cons (car elements) index)
                                 perm))
            ; 2. if we do things recursively, this will return
            ;      a list of all permutations
            (gen-permutation rest-elements rest-indices))))
      indices)))

(define (permutation-to-seq perm)
  (define (compare a b)
    ; index is the second element
    (< (cdr a) (cdr b)))
  (map car (sort perm compare)))

(define (remove-sorted-dup ls)
  (define (update acc i)
    (cond ((null? acc) (list i))
          ((equal? (car acc) i) acc)
          (else (cons i acc))))
  (fold-left update nil (reverse ls)))

(define (permu-compare< l1 l2)
  ; guaranteed that (length l1) = (length l2)
  (cond ((and (null? l1) (null? l2))
          #f)
        ((= (car l1) (car l2))
          (permu-compare< (cdr l1) (cdr l2)))
        ((< (car l1) (car l2))
          #t)
        (else #f)))

(define all-seqs
  (remove-sorted-dup
  (sort
    (map
      permutation-to-seq
      (gen-permutation '(0 0 1 1 2 2)
                       '(0 1 2 3 4 5)))
    permu-compare<)))

; step 3: model a person
(define (make-tasker modifier)
  (let ((my-balance nil)
        (stage 0))
    ; set my-balance to modified result
    (define (access bank-balance)
      (set! my-balance (modifier bank-balance)))
    (define (reset)
      (set! my-balance nil)
      (set! stage 0))
    ; call `step` to do the next task
    ;   this function is suppose to
    ;   return the current-balance after
    ;   performing this step
    (define (step current-balance)
      (cond ((= stage 0)
              (set! stage 1)
              (access current-balance)
              ; just read in, return it without change
              current-balance)
            ((= stage 1)
              (set! stage 2)
              ; modify it
              my-balance)
            (else (error "wrong stage"))))
    (define (dispatch m)
      (cond ((eq? m 'step) step)
            ((eq? m 'reset) reset)))
    dispatch))

(define (tasker-step t b)
  ((t 'step) b))
(define (tasker-reset t)
  ((t 'reset)))

(define peter (make-tasker (lambda (b) (+ b 10))))
(define paul  (make-tasker (lambda (b) (- b 20))))
(define mary  (make-tasker (lambda (b) (- b (/ b 2)))))

(define all-taskers (list peter paul mary))

; return final balance
(define (simulate-seq seq)
  ; reset taskers
  (for-each
    tasker-reset
    all-taskers)
  (define result
    (let loop ((s seq)
               (current-balance 100))
      ; s => who to wake up
      (if (null? s)
        current-balance
        (begin
          (let ((next-balance 
                  ; take one step
                  (tasker-step
                    (list-ref all-taskers (car s))
                    current-balance)))
            (loop (cdr s) next-balance))))))
  result)

(out "simple verification:")
(out (simulate-seq '(0 0 2 2 1 1))) ; ACB
(out "ACB => 35")
(out (simulate-seq '(1 1 0 0 2 2))) ; BAC
(out "BAC => 45")

(out "all possible results:")
(for-each
  (lambda (x)
    (format #t "seq: ~A, result: ~A~%"
            (car x)
            (cdr x)))
  (map
    (lambda (seq) (cons seq (simulate-seq seq)))
    all-seqs))

(end-script)
